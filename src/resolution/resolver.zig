//! Resolver interface and implementations.
//!
//! Provides a chain of resolvers that attempt to resolve import strings
//! to canonical module identifiers and file URIs.

const std = @import("std");
const builtin = @import("builtin");
const types = @import("types.zig");
const builtins = @import("builtins.zig");
const Config = types.Config;
const ResolveRequest = types.ResolveRequest;
const ResolveResult = types.ResolveResult;
const ResolverConfig = types.ResolverConfig;
const ResolverKind = types.ResolverKind;
const uri_util = @import("../uri.zig");

pub const log = std.log.scoped(.wren_lsp_resolver);

const is_wasm_target = builtin.target.cpu.arch == .wasm32 or builtin.target.cpu.arch == .wasm64;

/// Resolver interface - implemented by each resolver type.
pub const Resolver = struct {
    ptr: *anyopaque,
    vtable: *const VTable,
    stop_on_null: bool = false,

    pub const VTable = struct {
        resolve: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult,
        deinit: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator) void,
    };

    pub fn resolve(self: Resolver, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        return self.vtable.resolve(self.ptr, allocator, request);
    }

    pub fn deinit(self: Resolver, allocator: std.mem.Allocator) void {
        self.vtable.deinit(self.ptr, allocator);
    }
};

/// Chain of resolvers - tries each in order until one succeeds.
pub const ResolverChain = struct {
    allocator: std.mem.Allocator,
    resolvers: std.ArrayListUnmanaged(Resolver) = .empty,
    config: Config,

    pub fn init(allocator: std.mem.Allocator, config: Config) !ResolverChain {
        var chain = ResolverChain{
            .allocator = allocator,
            .config = config,
        };

        for (config.resolvers) |resolver_config| {
            if (try chain.createResolver(resolver_config)) |resolver| {
                try chain.resolvers.append(allocator, resolver);
            }
        }

        if (chain.resolvers.items.len == 0) {
            try chain.addDefaultResolvers();
        } else {
            try chain.addBuiltinResolver();
        }

        return chain;
    }

    pub fn deinit(self: *ResolverChain) void {
        for (self.resolvers.items) |resolver| {
            resolver.deinit(self.allocator);
        }
        self.resolvers.deinit(self.allocator);
    }

    fn addDefaultResolvers(self: *ResolverChain) !void {
        if (comptime is_wasm_target) {
            const host_resolver = try HostResolver.create(self.allocator, .{});
            try self.resolvers.append(self.allocator, host_resolver);
        }

        const path_resolver = try PathResolver.create(
            self.allocator,
            .{ .delimiter = "/" },
            self.config.modules,
            self.config.project_root,
        );
        try self.resolvers.append(self.allocator, path_resolver);

        try self.addBuiltinResolver();
    }

    fn addBuiltinResolver(self: *ResolverChain) !void {
        const builtin_resolver = try BuiltinResolver.create(self.allocator);
        try self.resolvers.append(self.allocator, builtin_resolver);
    }

    fn createResolver(self: *ResolverChain, cfg: ResolverConfig) !?Resolver {
        return switch (cfg.kind) {
            .path => blk: {
                break :blk try PathResolver.create(
                    self.allocator,
                    cfg.path,
                    self.config.modules,
                    self.config.project_root,
                );
            },
            .plugin => blk: {
                var resolver = try PluginResolver.create(self.allocator, cfg.plugin, self.config.project_root);
                resolver.stop_on_null = !cfg.plugin.fallback_to_builtin;
                break :blk resolver;
            },
            .host => blk: {
                var resolver = try HostResolver.create(self.allocator, cfg.host);
                resolver.stop_on_null = !cfg.host.fallback_to_builtin;
                break :blk resolver;
            },
        };
    }

    /// Resolve an import string to a module.
    pub fn resolve(self: *ResolverChain, request: ResolveRequest) ?ResolveResult {
        for (self.resolvers.items) |resolver| {
            if (resolver.resolve(self.allocator, request)) |result| {
                return result;
            }
            if (resolver.stop_on_null) {
                return null;
            }
        }

        const import_str = request.import_string;
        if (std.mem.startsWith(u8, import_str, "./") or
            std.mem.startsWith(u8, import_str, "../") or
            std.fs.path.isAbsolute(import_str))
        {
            return null;
        }

        return null;
    }
};

pub const HostResolver = struct {
    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, _: types.HostResolverConfig) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{};
        return .{
            .ptr = self,
            .vtable = &.{
                .resolve = &resolveImpl,
                .deinit = &deinitImpl,
            },
        };
    }

    fn resolveImpl(ptr: *anyopaque, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        _ = ptr;
        if (comptime !is_wasm_target) return null;
        return doResolve(allocator, request);
    }

    fn deinitImpl(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *Self = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }

    fn doResolve(allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        var cap: usize = 4096;

        while (cap <= 8 * 1024 * 1024) {
            const buf = allocator.alloc(u8, cap) catch return null;
            defer allocator.free(buf);

            const needed = wren_lsp_host_resolve(
                request.importer_uri.ptr,
                request.importer_uri.len,
                request.import_string.ptr,
                request.import_string.len,
                request.project_root.ptr,
                request.project_root.len,
                buf.ptr,
                buf.len,
            );

            if (needed == 0) return null;
            if (needed > buf.len) {
                cap = needed;
                continue;
            }

            return parseHostResolveResult(allocator, buf[0..needed]);
        }

        return null;
    }

    fn parseHostResolveResult(allocator: std.mem.Allocator, json_payload: []const u8) ?ResolveResult {
        var arena_state = std.heap.ArenaAllocator.init(allocator);
        defer arena_state.deinit();
        const arena = arena_state.allocator();

        const parsed = std.json.parseFromSlice(std.json.Value, arena, json_payload, .{}) catch return null;
        const root = parsed.value;
        if (root != .object) return null;
        const obj = root.object;

        const canonical_id = if (obj.get("canonical_id")) |v|
            if (v == .string) v.string else return null
        else if (obj.get("canonicalId")) |v|
            if (v == .string) v.string else return null
        else
            return null;

        const maybe_uri = if (obj.get("uri")) |v|
            if (v == .string) v.string else null
        else
            null;

        const maybe_source = if (obj.get("source")) |v|
            if (v == .string) v.string else null
        else
            null;

        const kind: ResolveResult.ModuleKind = if (obj.get("kind")) |v| blk: {
            if (v != .string) break :blk if (maybe_source != null) .virtual else .file;
            if (std.mem.eql(u8, v.string, "virtual")) break :blk .virtual;
            if (std.mem.eql(u8, v.string, "remote")) break :blk .remote;
            break :blk .file;
        } else if (maybe_source != null) .virtual else .file;

        return ResolveResult{
            .canonical_id = allocator.dupe(u8, canonical_id) catch return null,
            .uri = if (maybe_uri) |uri| allocator.dupe(u8, uri) catch null else null,
            .source = if (maybe_source) |source| allocator.dupe(u8, source) catch null else null,
            .kind = kind,
        };
    }
};

extern fn wren_lsp_host_resolve(
    importer_ptr: [*]const u8,
    importer_len: usize,
    import_ptr: [*]const u8,
    import_len: usize,
    project_root_ptr: [*]const u8,
    project_root_len: usize,
    out_ptr: [*]u8,
    out_cap: usize,
) usize;

/// Path resolver - treats import strings as file paths.
/// Supports custom delimiter for dotted imports (e.g., "game.physics" -> "game/physics.wren").
/// Also supports named modules which take priority over directory-based resolution.
pub const PathResolver = struct {
    roots: []const []const u8,
    named_modules: []const types.ModuleEntry.NamedModule,
    delimiter: []const u8,
    project_root: ?[]const u8,

    const Self = @This();

    pub fn create(
        allocator: std.mem.Allocator,
        cfg: types.PathResolverConfig,
        modules: []const types.ModuleEntry,
        project_root: ?[]const u8,
    ) !Resolver {
        // Extract directory roots and named modules from module entries
        var roots: std.ArrayListUnmanaged([]const u8) = .empty;
        var named: std.ArrayListUnmanaged(types.ModuleEntry.NamedModule) = .empty;

        for (modules) |entry| {
            switch (entry) {
                .directory => |d| try roots.append(allocator, d),
                .named => |n| try named.append(allocator, n),
            }
        }

        // Add explicit roots from config
        for (cfg.roots) |root| {
            try roots.append(allocator, root);
        }

        const self = try allocator.create(Self);
        self.* = .{
            .roots = try roots.toOwnedSlice(allocator),
            .named_modules = try named.toOwnedSlice(allocator),
            .delimiter = cfg.delimiter,
            .project_root = project_root,
        };
        return .{
            .ptr = self,
            .vtable = &.{
                .resolve = &resolveImpl,
                .deinit = &deinitImpl,
            },
        };
    }

    fn resolveImpl(ptr: *anyopaque, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const self: *Self = @ptrCast(@alignCast(ptr));
        return self.doResolve(allocator, request);
    }

    fn deinitImpl(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *Self = @ptrCast(@alignCast(ptr));
        allocator.free(self.roots);
        allocator.free(self.named_modules);
        allocator.destroy(self);
    }

    fn doResolve(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const import_str = request.import_string;

        // Handle relative imports
        if (std.mem.startsWith(u8, import_str, "./") or std.mem.startsWith(u8, import_str, "../")) {
            return self.resolveRelative(allocator, request);
        }

        // Handle absolute paths
        if (std.fs.path.isAbsolute(import_str)) {
            return self.resolveAbsolute(allocator, import_str);
        }

        // Named modules take priority over directory-based resolution
        if (self.resolveNamedModule(allocator, import_str)) |result| {
            return result;
        }

        // Handle delimiter-based imports (e.g., "game.physics" with delimiter ".")
        return self.resolveFromRoots(allocator, request);
    }

    fn resolveNamedModule(self: *Self, allocator: std.mem.Allocator, import_str: []const u8) ?ResolveResult {
        // Search from end so "last one wins" - matches merge semantics where child overrides parent
        var i: usize = self.named_modules.len;
        while (i > 0) {
            i -= 1;
            const named = self.named_modules[i];
            if (!std.mem.eql(u8, named.name, import_str)) continue;

            const base_dir = self.project_root orelse ".";
            const resolved_path = if (std.fs.path.isAbsolute(named.path))
                allocator.dupe(u8, named.path) catch return null
            else
                std.fs.path.join(allocator, &.{ base_dir, named.path }) catch return null;
            defer allocator.free(resolved_path);

            if (std.fs.cwd().access(resolved_path, .{ .mode = .read_only })) |_| {
                const real_path = canonicalPath(allocator, resolved_path) orelse return null;
                defer allocator.free(real_path);
                return ResolveResult{
                    .canonical_id = uri_util.pathToUri(allocator, real_path) catch return null,
                    .uri = uri_util.pathToUri(allocator, real_path) catch return null,
                    .kind = .file,
                };
            } else |_| {}
        }
        return null;
    }

    fn resolveRelative(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const importer_path = uriToPath(request.importer_uri);
        const importer_dir = std.fs.path.dirname(importer_path) orelse ".";

        return self.tryResolveFromBase(allocator, importer_dir, request.import_string);
    }

    fn resolveAbsolute(_: *Self, allocator: std.mem.Allocator, import_path: []const u8) ?ResolveResult {
        const with_ext_allocated = !std.mem.endsWith(u8, import_path, ".wren");
        const with_ext = if (with_ext_allocated)
            std.fmt.allocPrint(allocator, "{s}.wren", .{import_path}) catch return null
        else
            import_path;
        defer if (with_ext_allocated) allocator.free(with_ext);

        if (std.fs.cwd().access(with_ext, .{ .mode = .read_only })) |_| {
            return ResolveResult{
                .canonical_id = uri_util.pathToUri(allocator, with_ext) catch return null,
                .uri = uri_util.pathToUri(allocator, with_ext) catch return null,
                .kind = .file,
            };
        } else |_| {}

        return null;
    }

    fn resolveFromRoots(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const base_dir = if (self.project_root) |pr| pr else ".";
        const import_str = request.import_string;

        // Convert delimiter to platform path separator
        const path_str_allocated = !std.mem.eql(u8, self.delimiter, std.fs.path.sep_str) and std.mem.indexOf(u8, import_str, self.delimiter) != null;
        const path_str = if (path_str_allocated)
            std.mem.replaceOwned(u8, allocator, import_str, self.delimiter, std.fs.path.sep_str) catch return null
        else
            import_str;
        defer if (path_str_allocated) allocator.free(path_str);

        for (self.roots) |root| {
            const resolved_root_allocated = std.mem.startsWith(u8, root, "./");
            const resolved_root = if (resolved_root_allocated)
                std.fs.path.join(allocator, &.{ base_dir, root[2..] }) catch continue
            else
                root;
            defer if (resolved_root_allocated) allocator.free(resolved_root);

            if (self.tryResolveFromBase(allocator, resolved_root, path_str)) |result| {
                return result;
            }
        }

        return null;
    }

    fn tryResolveFromBase(_: *Self, allocator: std.mem.Allocator, base: []const u8, import_str: []const u8) ?ResolveResult {
        const with_ext_allocated = !std.mem.endsWith(u8, import_str, ".wren");
        const with_ext = if (with_ext_allocated)
            std.fmt.allocPrint(allocator, "{s}.wren", .{import_str}) catch return null
        else
            import_str;
        defer if (with_ext_allocated) allocator.free(with_ext);

        const full_path = std.fs.path.join(allocator, &.{ base, with_ext }) catch return null;
        defer allocator.free(full_path);

        if (std.fs.cwd().access(full_path, .{ .mode = .read_only })) |_| {
            const real_path = canonicalPath(allocator, full_path) orelse return null;
            defer allocator.free(real_path);
            return ResolveResult{
                .canonical_id = uri_util.pathToUri(allocator, real_path) catch return null,
                .uri = uri_util.pathToUri(allocator, real_path) catch return null,
                .kind = .file,
            };
        } else |_| {}

        return null;
    }

    fn canonicalPath(allocator: std.mem.Allocator, path: []const u8) ?[]u8 {
        if (comptime builtin.target.os.tag == .wasi) {
            return allocator.dupe(u8, path) catch null;
        }
        return std.fs.cwd().realpathAlloc(allocator, path) catch null;
    }
};

pub const BuiltinResolver = struct {
    const Self = @This();

    pub fn create(allocator: std.mem.Allocator) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{};
        return .{
            .ptr = self,
            .vtable = &.{
                .resolve = &resolveImpl,
                .deinit = &deinitImpl,
            },
        };
    }

    fn resolveImpl(ptr: *anyopaque, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        _ = ptr;
        const import_str = request.import_string;

        if (std.mem.startsWith(u8, import_str, "./") or
            std.mem.startsWith(u8, import_str, "../") or
            std.fs.path.isAbsolute(import_str))
        {
            return null;
        }

        if (builtins.getBuiltinSource(import_str)) |source| {
            return ResolveResult{
                .canonical_id = std.fmt.allocPrint(allocator, "wren://builtin/{s}", .{import_str}) catch return null,
                .uri = std.fmt.allocPrint(allocator, "wren://builtin/{s}", .{import_str}) catch return null,
                .source = source,
                .kind = .virtual,
            };
        }

        return null;
    }

    fn deinitImpl(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *Self = @ptrCast(@alignCast(ptr));
        allocator.destroy(self);
    }
};

/// Plugin resolver - calls external shared library with C ABI.
pub const PluginResolver = struct {
    library_path: []const u8,
    fallback_to_builtin: bool,
    handle: ?std.DynLib = null,
    resolve_fn: ?ResolveFn = null,
    free_fn: ?FreeFn = null,

    const ResolveFn = *const fn (
        importer_uri: [*:0]const u8,
        import_string: [*:0]const u8,
        project_root: [*:0]const u8,
    ) callconv(.c) WrenLspResolveResult;

    const FreeFn = *const fn (result: WrenLspResolveResult) callconv(.c) void;

    const Self = @This();

    /// Platform-specific shared library extension.
    const native_lib_ext = switch (@import("builtin").os.tag) {
        .windows => ".dll",
        .macos => ".dylib",
        else => ".so",
    };

    pub fn create(allocator: std.mem.Allocator, cfg: types.PluginResolverConfig, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .library_path = cfg.library,
            .fallback_to_builtin = cfg.fallback_to_builtin,
        };

        if (comptime is_wasm_target) {
            log.info("Plugin resolver disabled on wasm target", .{});
            return createResolver(self);
        }

        if (comptime !is_wasm_target) {
            const lib_path = resolveLibraryPath(allocator, cfg.library, project_root) orelse {
                log.warn("Failed to load plugin: could not resolve path", .{});
                return createResolver(self);
            };
            defer allocator.free(lib_path);

            self.handle = std.DynLib.open(lib_path) catch |err| {
                log.warn("Failed to load plugin library '{s}': {}", .{ lib_path, err });
                return createResolver(self);
            };

            if (self.handle) |*h| {
                self.resolve_fn = h.lookup(ResolveFn, "wren_lsp_resolve_module");
                self.free_fn = h.lookup(FreeFn, "wren_lsp_free_result");

                if (self.resolve_fn == null) {
                    log.warn("Plugin missing wren_lsp_resolve_module symbol", .{});
                }
                if (self.free_fn == null) {
                    log.warn("Plugin missing wren_lsp_free_result symbol", .{});
                }
            }
        }

        return createResolver(self);
    }

    fn createResolver(self: *Self) Resolver {
        return .{
            .ptr = self,
            .vtable = &.{
                .resolve = &resolveImpl,
                .deinit = &deinitImpl,
            },
        };
    }

    /// Resolve the library path, handling relative paths and platform extensions.
    fn resolveLibraryPath(allocator: std.mem.Allocator, library: []const u8, project_root: ?[]const u8) ?[]const u8 {
        // Resolve relative path against project root
        const base_path = if (std.mem.startsWith(u8, library, "./"))
            if (project_root) |pr|
                std.fs.path.join(allocator, &.{ pr, library[2..] }) catch return null
            else
                allocator.dupe(u8, library) catch return null
        else
            allocator.dupe(u8, library) catch return null;

        // Check if the path already has a recognized extension
        if (hasLibraryExtension(base_path)) {
            return base_path;
        }

        // Try appending platform-specific extension
        defer allocator.free(base_path);
        return std.fmt.allocPrint(allocator, "{s}{s}", .{ base_path, native_lib_ext }) catch null;
    }

    fn hasLibraryExtension(path: []const u8) bool {
        return std.mem.endsWith(u8, path, ".dylib") or
            std.mem.endsWith(u8, path, ".so") or
            std.mem.endsWith(u8, path, ".dll");
    }

    fn resolveImpl(ptr: *anyopaque, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const self: *Self = @ptrCast(@alignCast(ptr));
        return self.doResolve(allocator, request);
    }

    fn deinitImpl(ptr: *anyopaque, allocator: std.mem.Allocator) void {
        const self: *Self = @ptrCast(@alignCast(ptr));
        if (comptime !is_wasm_target) {
            if (self.handle) |*h| {
                h.close();
            }
        }
        allocator.destroy(self);
    }

    fn doResolve(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const resolve_fn = self.resolve_fn orelse return null;

        log.info("PluginResolver: resolve import='{s}'", .{request.import_string});

        const importer_uri = allocator.dupeZ(u8, request.importer_uri) catch return null;
        defer allocator.free(importer_uri);
        const import_string = allocator.dupeZ(u8, request.import_string) catch return null;
        defer allocator.free(import_string);
        const project_root = allocator.dupeZ(u8, request.project_root) catch return null;
        defer allocator.free(project_root);

        const response = resolve_fn(
            importer_uri.ptr,
            import_string.ptr,
            project_root.ptr,
        );
        defer if (self.free_fn) |free| free(response);

        if (response.canonical_id == null) {
            log.info("PluginResolver: unresolved import '{s}'", .{request.import_string});
            return null;
        }

        log.info("PluginResolver: resolved canonical_id='{s}'", .{std.mem.span(response.canonical_id.?)});

        var result = ResolveResult{
            .canonical_id = allocator.dupe(u8, std.mem.span(response.canonical_id.?)) catch return null,
        };

        if (response.uri) |uri| {
            result.uri = allocator.dupe(u8, std.mem.span(uri)) catch null;
        }

        if (response.source) |source| {
            result.source = allocator.dupe(u8, std.mem.span(source)) catch null;
        }

        if (response.kind) |kind| {
            const kind_str = std.mem.span(kind);
            if (std.mem.eql(u8, kind_str, "file")) {
                result.kind = .file;
            } else if (std.mem.eql(u8, kind_str, "virtual")) {
                result.kind = .virtual;
            } else if (std.mem.eql(u8, kind_str, "remote")) {
                result.kind = .remote;
            }
        }

        if (response.diagnostics != null and response.diagnostics_len > 0) {
            var diagnostics: std.ArrayListUnmanaged(ResolveResult.Diagnostic) = .empty;
            for (response.diagnostics.?[0..response.diagnostics_len]) |diag| {
                if (diag.message == null or diag.severity == null) continue;
                const message = allocator.dupe(u8, std.mem.span(diag.message.?)) catch continue;
                const severity = parseDiagnosticSeverityString(std.mem.span(diag.severity.?)) orelse {
                    allocator.free(message);
                    continue;
                };
                diagnostics.append(allocator, .{ .severity = severity, .message = message }) catch {
                    allocator.free(message);
                    continue;
                };
            }
            result.diagnostics = diagnostics.toOwnedSlice(allocator) catch result.diagnostics;
        }

        return result;
    }
};

const WrenLspDiagnostic = extern struct {
    severity: ?[*:0]const u8,
    message: ?[*:0]const u8,
};

const WrenLspResolveResult = extern struct {
    canonical_id: ?[*:0]const u8,
    uri: ?[*:0]const u8,
    source: ?[*:0]const u8,
    kind: ?[*:0]const u8,
    diagnostics: ?[*]const WrenLspDiagnostic,
    diagnostics_len: usize,
};

fn parseDiagnosticSeverityString(value: []const u8) ?ResolveResult.Diagnostic.Severity {
    if (std.mem.eql(u8, value, "error")) return .@"error";
    if (std.mem.eql(u8, value, "warning")) return .warning;
    if (std.mem.eql(u8, value, "info")) return .info;
    if (std.mem.eql(u8, value, "hint")) return .hint;
    return null;
}

const uriToPath = uri_util.uriToPath;
