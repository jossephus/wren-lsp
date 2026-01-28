//! Resolver interface and implementations.
//!
//! Provides a chain of resolvers that attempt to resolve import strings
//! to canonical module identifiers and file URIs.

const std = @import("std");
const types = @import("types.zig");
const Config = types.Config;
const ResolveRequest = types.ResolveRequest;
const ResolveResult = types.ResolveResult;
const ResolverConfig = types.ResolverConfig;
const ResolverKind = types.ResolverKind;

pub const log = std.log.scoped(.wren_lsp_resolver);

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
        const path_resolver = try PathResolver.create(self.allocator, .{
            .roots = if (self.config.modules.len > 0) self.config.modules else &.{"."},
            .delimiter = "/",
        }, self.config.project_root);
        try self.resolvers.append(self.allocator, path_resolver);
    }

    fn createResolver(self: *ResolverChain, cfg: ResolverConfig) !?Resolver {
        return switch (cfg.kind) {
            .path => blk: {
                // Use modules as default roots if no explicit roots configured
                var path_cfg = cfg.path;
                if (path_cfg.roots.len == 0 and self.config.modules.len > 0) {
                    path_cfg.roots = self.config.modules;
                }
                break :blk try PathResolver.create(self.allocator, path_cfg, self.config.project_root);
            },
            .plugin => blk: {
                var resolver = try PluginResolver.create(self.allocator, cfg.plugin, self.config.project_root);
                resolver.stop_on_null = !cfg.plugin.fallback_to_builtin;
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

        // Only return virtual fallback for non-path imports.
        // Relative paths (./foo, ../foo) and absolute paths should return null
        // so the caller can report a missing import diagnostic.
        const import_str = request.import_string;
        if (std.mem.startsWith(u8, import_str, "./") or
            std.mem.startsWith(u8, import_str, "../") or
            std.fs.path.isAbsolute(import_str))
        {
            return null;
        }

        return ResolveResult{
            .canonical_id = std.fmt.allocPrint(self.allocator, "wren://virtual/{s}", .{request.import_string}) catch return null,
            .uri = null,
            .kind = .virtual,
        };
    }
};

/// Path resolver - treats import strings as file paths.
/// Supports custom delimiter for dotted imports (e.g., "game.physics" -> "game/physics.wren").
pub const PathResolver = struct {
    roots: []const []const u8,
    delimiter: []const u8,
    project_root: ?[]const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, cfg: types.PathResolverConfig, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .roots = cfg.roots,
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

        // Handle delimiter-based imports (e.g., "game.physics" with delimiter ".")
        return self.resolveFromRoots(allocator, request);
    }

    fn resolveRelative(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const importer_path = uriToPath(request.importer_uri);
        const importer_dir = std.fs.path.dirname(importer_path) orelse ".";

        return self.tryResolveFromBase(allocator, importer_dir, request.import_string);
    }

    fn resolveAbsolute(_: *Self, allocator: std.mem.Allocator, import_path: []const u8) ?ResolveResult {
        const with_ext = if (std.mem.endsWith(u8, import_path, ".wren"))
            import_path
        else
            std.fmt.allocPrint(allocator, "{s}.wren", .{import_path}) catch return null;

        if (std.fs.cwd().access(with_ext, .{ .mode = .read_only })) |_| {
            return ResolveResult{
                .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{with_ext}) catch return null,
                .uri = std.fmt.allocPrint(allocator, "file://{s}", .{with_ext}) catch return null,
                .kind = .file,
            };
        } else |_| {}

        return null;
    }

    fn resolveFromRoots(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const base_dir = if (self.project_root) |pr| pr else ".";
        const import_str = request.import_string;

        // Convert delimiter to path separator if delimiter is not "/"
        const path_str = if (!std.mem.eql(u8, self.delimiter, "/") and std.mem.indexOf(u8, import_str, self.delimiter) != null)
            std.mem.replaceOwned(u8, allocator, import_str, self.delimiter, "/") catch return null
        else
            import_str;

        for (self.roots) |root| {
            const resolved_root = if (std.mem.startsWith(u8, root, "./"))
                std.fs.path.join(allocator, &.{ base_dir, root[2..] }) catch continue
            else
                root;

            if (self.tryResolveFromBase(allocator, resolved_root, path_str)) |result| {
                return result;
            }
        }

        return null;
    }

    fn tryResolveFromBase(_: *Self, allocator: std.mem.Allocator, base: []const u8, import_str: []const u8) ?ResolveResult {
        const with_ext = if (std.mem.endsWith(u8, import_str, ".wren"))
            import_str
        else
            std.fmt.allocPrint(allocator, "{s}.wren", .{import_str}) catch return null;

        const full_path = std.fs.path.join(allocator, &.{ base, with_ext }) catch return null;

        if (std.fs.cwd().access(full_path, .{ .mode = .read_only })) |_| {
            const real_path = std.fs.cwd().realpathAlloc(allocator, full_path) catch full_path;
            return ResolveResult{
                .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                .uri = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                .kind = .file,
            };
        } else |_| {}

        return null;
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
        importer_module_id: [*:0]const u8,
        import_string: [*:0]const u8,
        project_root: [*:0]const u8,
        module_bases: ?[*]const [*:0]const u8,
        module_bases_len: usize,
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
        if (self.handle) |*h| {
            h.close();
        }
        allocator.destroy(self);
    }

    fn doResolve(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const resolve_fn = self.resolve_fn orelse return null;

        log.info("PluginResolver: resolve import='{s}'", .{request.import_string});

        const importer_uri = allocator.dupeZ(u8, request.importer_uri) catch return null;
        defer allocator.free(importer_uri);
        const importer_module_id = allocator.dupeZ(u8, request.importer_module_id) catch return null;
        defer allocator.free(importer_module_id);
        const import_string = allocator.dupeZ(u8, request.import_string) catch return null;
        defer allocator.free(import_string);
        const project_root = allocator.dupeZ(u8, request.project_root) catch return null;
        defer allocator.free(project_root);

        const module_bases = buildModuleBasePaths(allocator, request.project_root, request.module_bases) catch return null;
        defer freeModuleBasePaths(allocator, module_bases);

        log.info("PluginResolver: module_bases_len={d}", .{module_bases.len});

        const response = resolve_fn(
            importer_uri.ptr,
            importer_module_id.ptr,
            import_string.ptr,
            project_root.ptr,
            if (module_bases.len > 0) @ptrCast(module_bases.ptr) else null,
            module_bases.len,
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
                const severity = parseDiagnosticSeverityString(std.mem.span(diag.severity.?)) orelse continue;
                diagnostics.append(allocator, .{ .severity = severity, .message = message }) catch continue;
            }
            result.diagnostics = diagnostics.toOwnedSlice(allocator) catch result.diagnostics;
        }

        if (response.completions != null and response.completions_len > 0) {
            var completions: std.ArrayListUnmanaged([]const u8) = .empty;
            for (response.completions.?[0..response.completions_len]) |item| {
                const value = allocator.dupe(u8, std.mem.span(item)) catch continue;
                completions.append(allocator, value) catch continue;
            }
            result.completions = completions.toOwnedSlice(allocator) catch result.completions;
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
    completions: ?[*]const [*:0]const u8,
    completions_len: usize,
};

fn buildModuleBasePaths(
    allocator: std.mem.Allocator,
    project_root: []const u8,
    module_bases: []const []const u8,
) ![]const [*:0]const u8 {
    if (module_bases.len == 0) return &.{};

    var bases = try allocator.alloc([*:0]const u8, module_bases.len);
    for (module_bases, 0..) |base, idx| {
        const resolved_base = if (std.mem.startsWith(u8, base, "./"))
            std.fs.path.join(allocator, &.{ project_root, base[2..] }) catch base
        else if (!std.fs.path.isAbsolute(base))
            std.fs.path.join(allocator, &.{ project_root, base }) catch base
        else
            base;

        defer if (resolved_base.ptr != base.ptr) allocator.free(resolved_base);
        bases[idx] = (try allocator.dupeZ(u8, resolved_base)).ptr;
    }

    return bases;
}

fn freeModuleBasePaths(allocator: std.mem.Allocator, bases: []const [*:0]const u8) void {
    if (bases.len == 0) return;
    for (bases) |base| {
        allocator.free(std.mem.span(base));
    }
    allocator.free(bases);
}

fn parseDiagnosticSeverityString(value: []const u8) ?ResolveResult.Diagnostic.Severity {
    if (std.mem.eql(u8, value, "error")) return .@"error";
    if (std.mem.eql(u8, value, "warning")) return .warning;
    if (std.mem.eql(u8, value, "info")) return .info;
    if (std.mem.eql(u8, value, "hint")) return .hint;
    return null;
}

fn uriToPath(uri: []const u8) []const u8 {
    const prefix = "file://";
    if (std.mem.startsWith(u8, uri, prefix)) {
        return uri[prefix.len..];
    }
    return uri;
}
