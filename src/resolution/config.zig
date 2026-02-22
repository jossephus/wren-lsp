//! Configuration file loader for wren-lsp.json.
//!
//! Searches upward from the opened file's directory to find the config file,
//! parses it, and caches the result by project root.

const std = @import("std");
const types = @import("types.zig");
const Config = types.Config;
const ResolverConfig = types.ResolverConfig;
const ResolverKind = types.ResolverKind;
const uri_util = @import("../uri.zig");

pub const log = std.log.scoped(.wren_lsp_config);

pub const ConfigLoader = struct {
    allocator: std.mem.Allocator,
    cache: std.StringHashMapUnmanaged(Config) = .empty,
    loading: std.StringHashMapUnmanaged(void) = .empty,

    pub fn init(allocator: std.mem.Allocator) ConfigLoader {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *ConfigLoader) void {
        var iter = self.cache.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            if (entry.value_ptr.config_path) |cp| {
                self.allocator.free(cp);
            }
            entry.value_ptr.deinit(self.allocator);
        }
        self.cache.deinit(self.allocator);
        self.loading.deinit(self.allocator);
    }

    pub fn loadForFile(self: *ConfigLoader, file_uri: []const u8) !Config {
        const file_path = uriToPath(file_uri);
        const dir = std.fs.path.dirname(file_path) orelse file_path;

        if (try self.findConfigFile(dir)) |config_path| {
            const project_root = std.fs.path.dirname(config_path) orelse config_path;

            if (self.cache.get(project_root)) |cached| {
                self.allocator.free(config_path);
                return cached;
            }

            var config = try self.loadConfigWithExtends(config_path);
            config.config_path = config_path;
            config.project_root = project_root;

            const key = try self.allocator.dupe(u8, project_root);
            try self.cache.put(self.allocator, key, config);
            return config;
        }

        return Config{};
    }

    fn loadConfigWithExtends(self: *ConfigLoader, config_path: []const u8) !Config {
        if (self.loading.contains(config_path)) {
            log.warn("Circular extends detected: {s}", .{config_path});
            return Config{};
        }

        try self.loading.put(self.allocator, config_path, {});
        defer _ = self.loading.remove(config_path);

        var config = try self.parseConfigFile(config_path);
        const config_dir = std.fs.path.dirname(config_path) orelse ".";

        // Normalize paths in this config relative to its directory
        try self.normalizePaths(&config, config_dir);

        // Process extends in order - later entries override earlier ones
        for (config.extends) |extends_path| {
            const parent_path = try self.resolveExtendsPath(config_dir, extends_path);

            if (parent_path) |pp| {
                defer self.allocator.free(pp);
                const parent_dir = std.fs.path.dirname(pp) orelse ".";
                var parent_config = try self.loadConfigWithExtends(pp);
                try self.normalizePaths(&parent_config, parent_dir);
                config = try mergeConfigs(self.allocator, parent_config, config);
            }
        }

        return config;
    }

    fn normalizePaths(self: *ConfigLoader, config: *Config, base_dir: []const u8) !void {
        const arena = if (config.arena) |a| a.allocator() else self.allocator;

        if (config.modules.len > 0) {
            var norm_modules = try std.ArrayListUnmanaged(types.ModuleEntry).initCapacity(arena, config.modules.len);
            for (config.modules) |entry| {
                const norm_entry: types.ModuleEntry = switch (entry) {
                    .directory => |d| .{ .directory = try joinAndNormalize(arena, base_dir, d) },
                    .named => |n| .{ .named = .{
                        .name = n.name,
                        .path = try joinAndNormalize(arena, base_dir, n.path),
                    } },
                };
                try norm_modules.append(arena, norm_entry);
            }
            config.modules = try norm_modules.toOwnedSlice(arena);
        }

        if (config.resolvers.len > 0) {
            var norm_resolvers = try arena.alloc(types.ResolverConfig, config.resolvers.len);
            for (config.resolvers, 0..) |resolver, i| {
                norm_resolvers[i] = resolver;
                if (resolver.kind == .path and resolver.path.roots.len > 0) {
                    var norm_roots = try std.ArrayListUnmanaged([]const u8).initCapacity(arena, resolver.path.roots.len);
                    for (resolver.path.roots) |root| {
                        const norm_path = try joinAndNormalize(arena, base_dir, root);
                        try norm_roots.append(arena, norm_path);
                    }
                    norm_resolvers[i].path.roots = try norm_roots.toOwnedSlice(arena);
                } else if (resolver.kind == .plugin and resolver.plugin.library.len > 0) {
                    norm_resolvers[i].plugin.library = try joinAndNormalize(arena, base_dir, resolver.plugin.library);
                } else if (resolver.kind == .host and resolver.host.script.len > 0) {
                    norm_resolvers[i].host.script = try joinAndNormalize(arena, base_dir, resolver.host.script);
                }
            }
            config.resolvers = norm_resolvers;
        }
    }

    fn resolveExtendsPath(self: *ConfigLoader, base_dir: []const u8, extends: []const u8) !?[]const u8 {
        const resolved = if (std.mem.startsWith(u8, extends, "./") or std.mem.startsWith(u8, extends, "../"))
            try std.fs.path.join(self.allocator, &.{ base_dir, extends })
        else if (std.fs.path.isAbsolute(extends))
            try self.allocator.dupe(u8, extends)
        else
            try std.fs.path.join(self.allocator, &.{ base_dir, extends });

        if (std.mem.endsWith(u8, resolved, ".json")) {
            if (std.fs.cwd().access(resolved, .{ .mode = .read_only })) |_| {
                return resolved;
            } else |_| {
                self.allocator.free(resolved);
                return null;
            }
        }

        const dir_config = try std.fs.path.join(self.allocator, &.{ resolved, "wren-lsp.json" });
        self.allocator.free(resolved);

        if (std.fs.cwd().access(dir_config, .{ .mode = .read_only })) |_| {
            return dir_config;
        } else |_| {
            self.allocator.free(dir_config);
            return null;
        }
    }

    fn findConfigFile(self: *ConfigLoader, start_dir: []const u8) !?[]const u8 {
        var current = start_dir;

        while (true) {
            const config_path = try std.fs.path.join(self.allocator, &.{ current, "wren-lsp.json" });

            if (std.fs.cwd().access(config_path, .{ .mode = .read_only })) |_| {
                return config_path;
            } else |_| {
                self.allocator.free(config_path);
            }

            if (std.fs.path.dirname(current)) |parent| {
                if (std.mem.eql(u8, parent, current)) break;
                current = parent;
            } else break;
        }

        return null;
    }

    fn parseConfigFile(self: *ConfigLoader, path: []const u8) !Config {
        const contents = std.fs.cwd().readFileAlloc(self.allocator, path, 1024 * 1024) catch |err| {
            log.warn("Failed to read config file {s}: {}", .{ path, err });
            return Config{};
        };
        defer self.allocator.free(contents);

        return self.parseJson(contents);
    }

    pub fn parseJson(self: *ConfigLoader, json_str: []const u8) !Config {
        const arena_ptr = try self.allocator.create(std.heap.ArenaAllocator);
        arena_ptr.* = std.heap.ArenaAllocator.init(self.allocator);
        const arena = arena_ptr.allocator();

        var config = Config{};
        config.arena = arena_ptr;

        const parsed = std.json.parseFromSlice(std.json.Value, arena, json_str, .{}) catch |err| {
            log.warn("Failed to parse config JSON: {}", .{err});
            return config;
        };

        const root = parsed.value;
        if (root != .object) return config;

        const obj = root.object;

        if (obj.get("version")) |v| {
            if (v == .integer) config.version = @intCast(v.integer);
        }

        if (obj.get("extends")) |v| {
            config.extends = try parseStringArrayAlloc(v, arena);
        }

        if (obj.get("modules")) |v| {
            config.modules = try parseModulesAlloc(v, arena);
        }

        if (obj.get("resolvers")) |v| {
            config.resolvers = try parseResolversAlloc(v, arena);
        }

        if (obj.get("diagnostics")) |v| {
            config.diagnostics = parseDiagnosticsConfig(v);
        }

        return config;
    }
};

fn mergeConfigs(allocator: std.mem.Allocator, parent: Config, child: Config) !Config {
    const arena = if (child.arena) |a| a.allocator() else allocator;

    // Concatenate parent and child modules
    var merged_modules: std.ArrayListUnmanaged(types.ModuleEntry) = .empty;
    for (parent.modules) |m| {
        try merged_modules.append(arena, m);
    }
    for (child.modules) |m| {
        try merged_modules.append(arena, m);
    }

    return Config{
        .version = child.version,
        .extends = &.{},
        .modules = try merged_modules.toOwnedSlice(arena),
        .resolvers = if (child.resolvers.len > 0) child.resolvers else parent.resolvers,
        .diagnostics = .{
            .missing_import = if (child.diagnostics.missing_import != .warning)
                child.diagnostics.missing_import
            else
                parent.diagnostics.missing_import,
            .extension_in_import = if (child.diagnostics.extension_in_import != .info)
                child.diagnostics.extension_in_import
            else
                parent.diagnostics.extension_in_import,
            .unknown_variable = if (child.diagnostics.unknown_variable != .@"error")
                child.diagnostics.unknown_variable
            else
                parent.diagnostics.unknown_variable,
        },
        .arena = child.arena,
    };
}

fn parseResolversAlloc(value: std.json.Value, arena: std.mem.Allocator) ![]const ResolverConfig {
    if (value != .array) return &.{};

    var resolvers: std.ArrayListUnmanaged(ResolverConfig) = .empty;

    for (value.array.items) |item| {
        if (try parseResolverAlloc(item, arena)) |resolver| {
            try resolvers.append(arena, resolver);
        }
    }

    return resolvers.toOwnedSlice(arena);
}

fn parseResolverAlloc(value: std.json.Value, arena: std.mem.Allocator) !?ResolverConfig {
    if (value != .object) return null;

    const obj = value.object;
    const type_str = obj.get("type") orelse return null;
    if (type_str != .string) return null;

    const kind = ResolverKind.fromString(type_str.string) orelse return null;

    var config = ResolverConfig{ .kind = kind };

    switch (kind) {
        .path => {
            config.path.roots = try parseStringArrayAlloc(obj.get("roots") orelse .null, arena);
            if (obj.get("delimiter")) |v| {
                if (v == .string) config.path.delimiter = try arena.dupe(u8, v.string);
            }
        },
        .plugin => {
            if (obj.get("library")) |v| {
                if (v == .string and v.string.len > 0) {
                    config.plugin.library = try arena.dupe(u8, v.string);
                } else {
                    return null;
                }
            } else {
                return null;
            }
            if (obj.get("fallbackToBuiltin")) |v| {
                if (v == .bool) config.plugin.fallback_to_builtin = v.bool;
            }
        },
        .host => {
            if (obj.get("script")) |v| {
                if (v == .string and v.string.len > 0) {
                    config.host.script = try arena.dupe(u8, v.string);
                }
            }
            if (obj.get("fallbackToBuiltin")) |v| {
                if (v == .bool) config.host.fallback_to_builtin = v.bool;
            }
        },
    }

    return config;
}

fn parseDiagnosticsConfig(value: std.json.Value) types.DiagnosticsConfig {
    var diag = types.DiagnosticsConfig{};
    if (value != .object) return diag;

    const obj = value.object;

    if (obj.get("missingImport")) |v| {
        if (v == .string) diag.missing_import = types.DiagnosticSeverity.fromString(v.string);
    }
    if (obj.get("extensionInImport")) |v| {
        if (v == .string) diag.extension_in_import = types.DiagnosticSeverity.fromString(v.string);
    }
    if (obj.get("unknownVariable")) |v| {
        if (v == .string) diag.unknown_variable = types.DiagnosticSeverity.fromString(v.string);
    }

    return diag;
}

fn parseStringArrayAlloc(value: std.json.Value, arena: std.mem.Allocator) ![]const []const u8 {
    if (value != .array) return &.{};

    var strings: std.ArrayListUnmanaged([]const u8) = .empty;

    for (value.array.items) |item| {
        if (item == .string) {
            try strings.append(arena, try arena.dupe(u8, item.string));
        }
    }

    return strings.toOwnedSlice(arena);
}

/// Parse modules array supporting both strings and named module objects.
/// Strings are treated as directory paths. Objects must have exactly one key
/// with a non-empty string value, mapping a module name to a file path.
fn parseModulesAlloc(value: std.json.Value, arena: std.mem.Allocator) ![]const types.ModuleEntry {
    if (value != .array) return &.{};

    var entries: std.ArrayListUnmanaged(types.ModuleEntry) = .empty;

    for (value.array.items) |item| {
        switch (item) {
            .string => |s| {
                if (s.len == 0) continue;
                try entries.append(arena, .{ .directory = try arena.dupe(u8, s) });
            },
            .object => |obj| {
                // Named modules must have exactly one key
                if (obj.count() != 1) {
                    log.warn("modules entry object must have exactly 1 key, got {d}", .{obj.count()});
                    continue;
                }
                var iter = obj.iterator();
                const kv = iter.next() orelse continue;

                const name = kv.key_ptr.*;
                if (name.len == 0) {
                    log.warn("modules entry has empty module name", .{});
                    continue;
                }

                if (kv.value_ptr.* != .string) {
                    log.warn("modules entry value must be a string path", .{});
                    continue;
                }
                const path = kv.value_ptr.string;
                if (path.len == 0) {
                    log.warn("modules entry has empty path for '{s}'", .{name});
                    continue;
                }

                try entries.append(arena, .{
                    .named = .{
                        .name = try arena.dupe(u8, name),
                        .path = try arena.dupe(u8, path),
                    },
                });
            },
            else => {},
        }
    }

    return entries.toOwnedSlice(arena);
}

/// Join base_dir with path and normalize, preserving path semantics.
fn joinAndNormalize(arena: std.mem.Allocator, base_dir: []const u8, path: []const u8) ![]const u8 {
    if (std.fs.path.isAbsolute(path)) {
        return try normalizePath(arena, path);
    }

    const joined = try std.fs.path.join(arena, &.{ base_dir, path });
    defer arena.free(joined);
    return try normalizePath(arena, joined);
}

/// Normalize a path by resolving "." and ".." components.
/// Preserves leading "./" for relative imports and absolute prefixes.
fn normalizePath(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    if (path.len == 0) return try allocator.dupe(u8, ".");

    var components = std.ArrayListUnmanaged([]const u8){};
    defer components.deinit(allocator);

    var leading_doubles: usize = 0;
    const is_absolute = std.fs.path.isAbsolute(path);
    const has_leading_dot_slash = std.mem.startsWith(u8, path, "./");

    var iter = std.mem.splitAny(u8, path, "/\\");
    while (iter.next()) |component| {
        if (component.len == 0) continue;

        if (std.mem.eql(u8, component, ".")) {
            continue;
        } else if (std.mem.eql(u8, component, "..")) {
            if (components.items.len > 0) {
                _ = components.pop();
            } else if (!is_absolute) {
                leading_doubles += 1;
            }
        } else {
            try components.append(allocator, component);
        }
    }

    var result = std.ArrayListUnmanaged(u8){};
    errdefer result.deinit(allocator);

    if (is_absolute) {
        try result.append(allocator, '/');
    } else if (leading_doubles > 0) {
        for (0..leading_doubles) |i| {
            if (i > 0) try result.append(allocator, '/');
            try result.appendSlice(allocator, "..");
        }
        if (components.items.len > 0) try result.append(allocator, '/');
    } else if (has_leading_dot_slash) {
        try result.appendSlice(allocator, "./");
    }

    for (components.items, 0..) |component, i| {
        if (i > 0) try result.append(allocator, '/');
        try result.appendSlice(allocator, component);
    }

    if (result.items.len == 0) {
        try result.append(allocator, '.');
    }

    return try result.toOwnedSlice(allocator);
}

pub fn freeConfig(backing_allocator: std.mem.Allocator, cfg: *Config) void {
    cfg.deinit(backing_allocator);
}

const uriToPath = uri_util.uriToPath;

test "parse empty config" {
    var loader = ConfigLoader.init(std.testing.allocator);
    defer loader.deinit();

    var config = try loader.parseJson("{}");
    defer config.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(u32, 1), config.version);
}

test "parse full config" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var loader = ConfigLoader.init(arena);

    const json =
        \\{
        \\  "version": 1,
        \\  "modules": ["./src", "./deps"],
        \\  "resolvers": [
        \\    {"type": "path", "roots": ["./src"], "delimiter": "."},
        \\    {"type": "plugin", "library": "./resolver.dylib", "fallbackToBuiltin": true}
        \\  ],
        \\  "diagnostics": {
        \\    "missingImport": "error"
        \\  }
        \\}
    ;

    const cfg = try loader.parseJson(json);

    try std.testing.expectEqual(@as(u32, 1), cfg.version);
    try std.testing.expectEqual(@as(usize, 2), cfg.modules.len);
    try std.testing.expectEqual(types.ModuleEntry.directory, std.meta.activeTag(cfg.modules[0]));
    try std.testing.expectEqualStrings("./src", cfg.modules[0].directory);
    try std.testing.expectEqual(@as(usize, 2), cfg.resolvers.len);
    try std.testing.expectEqualStrings(".", cfg.resolvers[0].path.delimiter);
    try std.testing.expectEqual(types.DiagnosticSeverity.@"error", cfg.diagnostics.missing_import);
}

test "parse modules with mixed syntax" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var loader = ConfigLoader.init(arena);

    const json =
        \\{
        \\  "modules": [
        \\    "./src",
        \\    {"sample-name": "./src/io.wren"},
        \\    {"another-mod": "./lib/utils.wren"}
        \\  ]
        \\}
    ;

    const cfg = try loader.parseJson(json);

    try std.testing.expectEqual(@as(usize, 3), cfg.modules.len);

    try std.testing.expectEqual(types.ModuleEntry.directory, std.meta.activeTag(cfg.modules[0]));
    try std.testing.expectEqualStrings("./src", cfg.modules[0].directory);

    try std.testing.expectEqual(types.ModuleEntry.named, std.meta.activeTag(cfg.modules[1]));
    try std.testing.expectEqualStrings("sample-name", cfg.modules[1].named.name);
    try std.testing.expectEqualStrings("./src/io.wren", cfg.modules[1].named.path);

    try std.testing.expectEqual(types.ModuleEntry.named, std.meta.activeTag(cfg.modules[2]));
    try std.testing.expectEqualStrings("another-mod", cfg.modules[2].named.name);
    try std.testing.expectEqualStrings("./lib/utils.wren", cfg.modules[2].named.path);
}

test "parse config with extends" {
    var loader = ConfigLoader.init(std.testing.allocator);
    defer loader.deinit();

    const json =
        \\{
        \\  "extends": ["./base/wren-lsp.json", "../shared"],
        \\  "modules": ["./src"]
        \\}
    ;

    var cfg = try loader.parseJson(json);
    defer cfg.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 2), cfg.extends.len);
    try std.testing.expectEqualStrings("./base/wren-lsp.json", cfg.extends[0]);
    try std.testing.expectEqualStrings("../shared", cfg.extends[1]);
    try std.testing.expectEqual(@as(usize, 1), cfg.modules.len);
}

test "normalizePath handles dot components" {
    const result = try normalizePath(std.testing.allocator, "./foo/bar/../baz");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("./foo/baz", result);
}

test "normalizePath preserves leading dot-slash" {
    const result = try normalizePath(std.testing.allocator, "./src/lib");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("./src/lib", result);
}

test "normalizePath handles parent refs" {
    const result = try normalizePath(std.testing.allocator, "../project/modules");
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("../project/modules", result);
}
