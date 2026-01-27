//! Configuration file loader for wren-lsp.json.
//!
//! Searches upward from the opened file's directory to find the config file,
//! parses it, and caches the result by project root.

const std = @import("std");
const types = @import("types.zig");
const Config = types.Config;
const ResolverConfig = types.ResolverConfig;
const ResolverKind = types.ResolverKind;

pub const log = std.log.scoped(.wren_lsp_config);

pub const ConfigLoader = struct {
    allocator: std.mem.Allocator,
    cache: std.StringHashMapUnmanaged(Config) = .empty,

    pub fn init(allocator: std.mem.Allocator) ConfigLoader {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *ConfigLoader) void {
        var iter = self.cache.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.cache.deinit(self.allocator);
    }

    /// Load config for the given file URI. Searches upward for wren-lsp.json.
    pub fn loadForFile(self: *ConfigLoader, file_uri: []const u8) !Config {
        const file_path = uriToPath(file_uri);
        const dir = std.fs.path.dirname(file_path) orelse file_path;

        if (try self.findConfigFile(dir)) |config_path| {
            const project_root = std.fs.path.dirname(config_path) orelse config_path;

            if (self.cache.get(project_root)) |cached| {
                return cached;
            }

            var config = try self.parseConfigFile(config_path);
            config.config_path = config_path;
            config.project_root = project_root;

            const key = try self.allocator.dupe(u8, project_root);
            try self.cache.put(self.allocator, key, config);
            return config;
        }

        return Config{};
    }

    /// Search upward from start_dir for wren-lsp.json.
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

    /// Parse wren-lsp.json file.
    fn parseConfigFile(self: *ConfigLoader, path: []const u8) !Config {
        const contents = std.fs.cwd().readFileAlloc(self.allocator, path, 1024 * 1024) catch |err| {
            log.warn("Failed to read config file {s}: {}", .{ path, err });
            return Config{};
        };
        defer self.allocator.free(contents);

        return self.parseJson(contents);
    }

    /// Parse JSON config content.
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
        // No defer deinit - parsed data lives in arena

        const root = parsed.value;
        if (root != .object) return config;

        const obj = root.object;

        if (obj.get("version")) |v| {
            if (v == .integer) config.version = @intCast(v.integer);
        }

        if (obj.get("workspace")) |ws| {
            config.workspace = try self.parseWorkspaceWithArena(ws, arena);
        }

        if (obj.get("imports")) |imp| {
            config.imports = try self.parseImportsWithArena(imp, arena);
        }

        return config;
    }

    fn parseWorkspaceWithArena(_: *ConfigLoader, value: std.json.Value, arena: std.mem.Allocator) !types.WorkspaceConfig {
        var ws = types.WorkspaceConfig{};
        if (value != .object) return ws;

        const obj = value.object;

        ws.roots = try parseStringArrayAlloc(obj.get("roots"), arena);
        ws.library = try parseStringArrayAlloc(obj.get("library"), arena);
        ws.stubs = try parseStringArrayAlloc(obj.get("stubs"), arena);
        ws.exclude = try parseStringArrayAlloc(obj.get("exclude"), arena);

        return ws;
    }

    fn parseImportsWithArena(_: *ConfigLoader, value: std.json.Value, arena: std.mem.Allocator) !types.ImportsConfig {
        var imp = types.ImportsConfig{};
        if (value != .object) return imp;

        const obj = value.object;

        if (obj.get("preferRelativeFromImporter")) |v| {
            if (v == .bool) imp.prefer_relative_from_importer = v.bool;
        }

        if (obj.get("resolvers")) |resolvers_val| {
            imp.resolvers = try parseResolversAlloc(resolvers_val, arena);
        }

        if (obj.get("diagnostics")) |diag| {
            imp.diagnostics = parseDiagnosticsConfigPure(diag);
        }

        return imp;
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
                config.path.roots = try parseStringArrayAlloc(obj.get("roots"), arena);
                config.path.extensions = try parseStringArrayAlloc(obj.get("extensions"), arena);
                config.path.index_files = try parseStringArrayAlloc(obj.get("indexFiles"), arena);
                if (config.path.extensions.len == 0) {
                    config.path.extensions = try dupeStringArrayAlloc(&.{".wren"}, arena);
                }
            },
            .dotted => {
                if (obj.get("delimiter")) |v| {
                    if (v == .string) config.dotted.delimiter = try arena.dupe(u8, v.string);
                }
                if (obj.get("mapTo")) |v| {
                    if (v == .string) config.dotted.map_to = try arena.dupe(u8, v.string);
                }
                config.dotted.roots = try parseStringArrayAlloc(obj.get("roots"), arena);
                config.dotted.extensions = try parseStringArrayAlloc(obj.get("extensions"), arena);
                if (config.dotted.extensions.len == 0) {
                    config.dotted.extensions = try dupeStringArrayAlloc(&.{".wren"}, arena);
                }
            },
            .scheme => {
                if (obj.get("scheme")) |v| {
                    if (v == .string) config.scheme.scheme = try arena.dupe(u8, v.string);
                }
                if (obj.get("stripPrefix")) |v| {
                    if (v == .string) config.scheme.strip_prefix = try arena.dupe(u8, v.string);
                }
            },
            .plugin => {
                if (obj.get("library")) |v| {
                    if (v == .string) config.plugin.library = try arena.dupe(u8, v.string);
                }
                if (obj.get("fallbackToBuiltin")) |v| {
                    if (v == .bool) config.plugin.fallback_to_builtin = v.bool;
                }
            },
            .stubs => {},
        }

        return config;
    }

    fn parseDiagnosticsConfigPure(value: std.json.Value) types.DiagnosticsConfig {
        var diag = types.DiagnosticsConfig{};
        if (value != .object) return diag;

        const obj = value.object;

        if (obj.get("missingImport")) |v| {
            if (v == .string) diag.missing_import = types.DiagnosticSeverity.fromString(v.string);
        }
        if (obj.get("unknownScheme")) |v| {
            if (v == .string) diag.unknown_scheme = types.DiagnosticSeverity.fromString(v.string);
        }
        if (obj.get("extensionInImport")) |v| {
            if (v == .string) diag.extension_in_import = types.DiagnosticSeverity.fromString(v.string);
        }

        return diag;
    }
};

fn parseStringArrayAlloc(value: ?std.json.Value, arena: std.mem.Allocator) ![]const []const u8 {
    const val = value orelse return &.{};
    if (val != .array) return &.{};

    var strings: std.ArrayListUnmanaged([]const u8) = .empty;

    for (val.array.items) |item| {
        if (item == .string) {
            try strings.append(arena, try arena.dupe(u8, item.string));
        }
    }

    return strings.toOwnedSlice(arena);
}

fn dupeStringArrayAlloc(src: []const []const u8, arena: std.mem.Allocator) ![]const []const u8 {
    const result = try arena.alloc([]const u8, src.len);
    for (src, 0..) |s, i| {
        result[i] = try arena.dupe(u8, s);
    }
    return result;
}

pub fn freeConfig(backing_allocator: std.mem.Allocator, cfg: *Config) void {
    cfg.deinit(backing_allocator);
}

fn uriToPath(uri: []const u8) []const u8 {
    const prefix = "file://";
    if (std.mem.startsWith(u8, uri, prefix)) {
        return uri[prefix.len..];
    }
    return uri;
}

test "parse empty config" {
    var loader = ConfigLoader.init(std.testing.allocator);
    defer loader.deinit();

    const config = try loader.parseJson("{}");
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
        \\  "workspace": {
        \\    "roots": ["./src"],
        \\    "stubs": ["builtin:wren-cli"]
        \\  },
        \\  "imports": {
        \\    "preferRelativeFromImporter": true,
        \\    "resolvers": [
        \\      {"type": "path", "roots": ["./src"], "extensions": [".wren"]},
        \\      {"type": "stubs"}
        \\    ],
        \\    "diagnostics": {
        \\      "missingImport": "error"
        \\    }
        \\  }
        \\}
    ;

    const cfg = try loader.parseJson(json);

    try std.testing.expectEqual(@as(u32, 1), cfg.version);
    try std.testing.expectEqual(@as(usize, 1), cfg.workspace.roots.len);
    try std.testing.expectEqual(@as(usize, 2), cfg.imports.resolvers.len);
    try std.testing.expectEqual(types.DiagnosticSeverity.@"error", cfg.imports.diagnostics.missing_import);
}
