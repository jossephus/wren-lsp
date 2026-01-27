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

        for (config.imports.resolvers) |resolver_config| {
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
            .roots = if (self.config.workspace.roots.len > 0) self.config.workspace.roots else &.{"."},
            .extensions = &.{".wren"},
            .index_files = &.{},
        }, self.config.project_root);
        try self.resolvers.append(self.allocator, path_resolver);

        const stubs_resolver = try StubsResolver.create(self.allocator, self.config.workspace.stubs, self.config.project_root);
        try self.resolvers.append(self.allocator, stubs_resolver);
    }

    fn createResolver(self: *ResolverChain, cfg: ResolverConfig) !?Resolver {
        return switch (cfg.kind) {
            .path => try PathResolver.create(self.allocator, cfg.path, self.config.project_root),
            .dotted => try DottedResolver.create(self.allocator, cfg.dotted, self.config.project_root),
            .scheme => try SchemeResolver.create(self.allocator, cfg.scheme, self.config.project_root),
            .stubs => try StubsResolver.create(self.allocator, self.config.workspace.stubs, self.config.project_root),
            .plugin => try PluginResolver.create(self.allocator, cfg.plugin, self.config.project_root),
        };
    }

    /// Resolve an import string to a module.
    pub fn resolve(self: *ResolverChain, request: ResolveRequest) ?ResolveResult {
        for (self.resolvers.items) |resolver| {
            if (resolver.resolve(self.allocator, request)) |result| {
                return result;
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
pub const PathResolver = struct {
    roots: []const []const u8,
    extensions: []const []const u8,
    index_files: []const []const u8,
    project_root: ?[]const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, cfg: types.PathResolverConfig, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .roots = cfg.roots,
            .extensions = cfg.extensions,
            .index_files = cfg.index_files,
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

        if (std.mem.startsWith(u8, import_str, "./") or std.mem.startsWith(u8, import_str, "../")) {
            return self.resolveRelative(allocator, request);
        }

        if (std.fs.path.isAbsolute(import_str)) {
            return self.resolveAbsolute(allocator, import_str);
        }

        return self.resolveFromRoots(allocator, request);
    }

    fn resolveRelative(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const importer_path = uriToPath(request.importer_uri);
        const importer_dir = std.fs.path.dirname(importer_path) orelse ".";

        return self.tryResolveFromBase(allocator, importer_dir, request.import_string);
    }

    fn resolveAbsolute(self: *Self, allocator: std.mem.Allocator, import_path: []const u8) ?ResolveResult {
        for (self.extensions) |ext| {
            const with_ext = if (std.mem.endsWith(u8, import_path, ext))
                import_path
            else
                std.fmt.allocPrint(allocator, "{s}{s}", .{ import_path, ext }) catch continue;

            if (std.fs.cwd().access(with_ext, .{ .mode = .read_only })) |_| {
                return ResolveResult{
                    .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{with_ext}) catch return null,
                    .uri = std.fmt.allocPrint(allocator, "file://{s}", .{with_ext}) catch return null,
                    .kind = .file,
                };
            } else |_| {}
        }

        return null;
    }

    fn resolveFromRoots(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const base_dir = if (self.project_root) |pr| pr else ".";

        for (self.roots) |root| {
            const resolved_root = if (std.mem.startsWith(u8, root, "./"))
                std.fs.path.join(allocator, &.{ base_dir, root[2..] }) catch continue
            else
                root;

            if (self.tryResolveFromBase(allocator, resolved_root, request.import_string)) |result| {
                return result;
            }
        }

        return null;
    }

    fn tryResolveFromBase(self: *Self, allocator: std.mem.Allocator, base: []const u8, import_str: []const u8) ?ResolveResult {
        for (self.extensions) |ext| {
            const with_ext = if (std.mem.endsWith(u8, import_str, ext))
                import_str
            else
                std.fmt.allocPrint(allocator, "{s}{s}", .{ import_str, ext }) catch continue;

            const full_path = std.fs.path.join(allocator, &.{ base, with_ext }) catch continue;

            if (std.fs.cwd().access(full_path, .{ .mode = .read_only })) |_| {
                const real_path = std.fs.cwd().realpathAlloc(allocator, full_path) catch full_path;
                return ResolveResult{
                    .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                    .uri = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                    .kind = .file,
                };
            } else |_| {}
        }

        for (self.index_files) |index| {
            const dir_path = std.fs.path.join(allocator, &.{ base, import_str }) catch continue;
            const index_path = std.fs.path.join(allocator, &.{ dir_path, index }) catch continue;

            if (std.fs.cwd().access(index_path, .{ .mode = .read_only })) |_| {
                const real_path = std.fs.cwd().realpathAlloc(allocator, index_path) catch index_path;
                return ResolveResult{
                    .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                    .uri = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                    .kind = .file,
                };
            } else |_| {}
        }

        return null;
    }
};

/// Dotted resolver - converts a.b.c to a/b/c.
pub const DottedResolver = struct {
    delimiter: []const u8,
    map_to: []const u8,
    roots: []const []const u8,
    extensions: []const []const u8,
    project_root: ?[]const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, cfg: types.DottedResolverConfig, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .delimiter = cfg.delimiter,
            .map_to = cfg.map_to,
            .roots = cfg.roots,
            .extensions = cfg.extensions,
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

        if (std.mem.indexOf(u8, import_str, self.delimiter) == null) {
            return null;
        }

        if (std.mem.startsWith(u8, import_str, "./") or std.mem.startsWith(u8, import_str, "../")) {
            return null;
        }

        const path_str = std.mem.replaceOwned(u8, allocator, import_str, self.delimiter, self.map_to) catch return null;

        const base_dir = if (self.project_root) |pr| pr else ".";

        for (self.roots) |root| {
            const resolved_root = if (std.mem.startsWith(u8, root, "./"))
                std.fs.path.join(allocator, &.{ base_dir, root[2..] }) catch continue
            else
                root;

            for (self.extensions) |ext| {
                const with_ext = std.fmt.allocPrint(allocator, "{s}{s}", .{ path_str, ext }) catch continue;
                const full_path = std.fs.path.join(allocator, &.{ resolved_root, with_ext }) catch continue;

                if (std.fs.cwd().access(full_path, .{ .mode = .read_only })) |_| {
                    const real_path = std.fs.cwd().realpathAlloc(allocator, full_path) catch full_path;
                    return ResolveResult{
                        .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                        .uri = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                        .kind = .file,
                    };
                } else |_| {}
            }
        }

        return null;
    }
};

/// Scheme resolver - handles prefixed imports like "hl:foo".
/// Returns a virtual module for scheme-prefixed imports.
pub const SchemeResolver = struct {
    scheme: []const u8,
    strip_prefix: []const u8,
    project_root: ?[]const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, cfg: types.SchemeResolverConfig, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .scheme = cfg.scheme,
            .strip_prefix = cfg.strip_prefix,
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

        if (!std.mem.startsWith(u8, import_str, self.strip_prefix)) {
            return null;
        }

        const remainder = import_str[self.strip_prefix.len..];
        const trimmed = std.mem.trim(u8, remainder, " ");

        return ResolveResult{
            .canonical_id = std.fmt.allocPrint(allocator, "wren://{s}/{s}", .{ self.scheme, trimmed }) catch return null,
            .uri = null,
            .kind = .virtual,
        };
    }
};

/// Stubs resolver - resolves built-in modules from stub directories.
pub const StubsResolver = struct {
    stubs_dirs: []const []const u8,
    project_root: ?[]const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, stubs_dirs: []const []const u8, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .stubs_dirs = stubs_dirs,
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
        log.info("StubsResolver: resolving '{s}', stubs_dirs.len={d}, project_root={s}", .{
            import_str,
            self.stubs_dirs.len,
            self.project_root orelse "(null)",
        });

        if (std.mem.startsWith(u8, import_str, "./") or std.mem.startsWith(u8, import_str, "../")) {
            log.info("StubsResolver: skipping relative import '{s}'", .{import_str});
            return null;
        }

        const base_dir = self.project_root orelse ".";

        for (self.stubs_dirs) |stub_dir| {
            log.info("StubsResolver: checking stub_dir '{s}'", .{stub_dir});

            if (std.mem.startsWith(u8, stub_dir, "builtin:")) {
                const builtin_name = stub_dir["builtin:".len..];
                log.info("StubsResolver: returning builtin virtual for '{s}'", .{builtin_name});
                return ResolveResult{
                    .canonical_id = std.fmt.allocPrint(allocator, "wren://builtin/{s}/{s}", .{ builtin_name, import_str }) catch return null,
                    .uri = null,
                    .kind = .virtual,
                };
            }

            const resolved_stub_dir = if (std.mem.startsWith(u8, stub_dir, "./"))
                std.fs.path.join(allocator, &.{ base_dir, stub_dir[2..] }) catch continue
            else
                stub_dir;
            log.info("StubsResolver: resolved_stub_dir='{s}'", .{resolved_stub_dir});

            const stub_path = std.fs.path.join(allocator, &.{ resolved_stub_dir, import_str }) catch continue;
            const with_ext = std.fmt.allocPrint(allocator, "{s}.wren", .{stub_path}) catch continue;
            log.info("StubsResolver: checking file '{s}'", .{with_ext});

            if (std.fs.cwd().access(with_ext, .{ .mode = .read_only })) |_| {
                const real_path = std.fs.cwd().realpathAlloc(allocator, with_ext) catch with_ext;
                log.info("StubsResolver: FOUND stub file '{s}'", .{real_path});
                return ResolveResult{
                    .canonical_id = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                    .uri = std.fmt.allocPrint(allocator, "file://{s}", .{real_path}) catch return null,
                    .kind = .file,
                };
            } else |err| {
                log.info("StubsResolver: file not found '{s}': {s}", .{ with_ext, @errorName(err) });
            }
        }

        log.info("StubsResolver: no match for '{s}'", .{import_str});
        return null;
    }
};

/// Plugin resolver - calls external shared library with C ABI.
pub const PluginResolver = struct {
    library_path: []const u8,
    fallback_to_builtin: bool,
    handle: ?*anyopaque = null,
    resolve_fn: ?*const fn ([*:0]const u8) callconv(.c) ?[*:0]const u8 = null,
    free_fn: ?*const fn ([*:0]const u8) callconv(.c) void = null,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, cfg: types.PluginResolverConfig, project_root: ?[]const u8) !Resolver {
        const self = try allocator.create(Self);
        self.* = .{
            .library_path = cfg.library,
            .fallback_to_builtin = cfg.fallback_to_builtin,
        };

        const lib_path = if (std.mem.startsWith(u8, cfg.library, "./"))
            if (project_root) |pr|
                std.fs.path.join(allocator, &.{ pr, cfg.library[2..] }) catch cfg.library
            else
                cfg.library
        else
            cfg.library;

        const lib_path_z = allocator.dupeZ(u8, lib_path) catch {
            log.warn("Failed to load plugin: could not allocate path", .{});
            return .{
                .ptr = self,
                .vtable = &.{
                    .resolve = &resolveImpl,
                    .deinit = &deinitImpl,
                },
            };
        };
        defer allocator.free(lib_path_z);

        self.handle = std.c.dlopen(lib_path_z.ptr, .{ .LAZY = true });
        if (self.handle) |h| {
            self.resolve_fn = @ptrCast(@alignCast(std.c.dlsym(h, "wren_lsp_resolve_module")));
            self.free_fn = @ptrCast(@alignCast(std.c.dlsym(h, "wren_lsp_free")));

            if (self.resolve_fn == null) {
                log.warn("Plugin missing wren_lsp_resolve_module symbol", .{});
            }
        } else {
            log.warn("Failed to load plugin library: {s}", .{lib_path});
        }

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
        if (self.handle) |h| {
            _ = std.c.dlclose(h);
        }
        allocator.destroy(self);
    }

    fn doResolve(self: *Self, allocator: std.mem.Allocator, request: ResolveRequest) ?ResolveResult {
        const resolve_fn = self.resolve_fn orelse return null;

        const request_json_slice = std.fmt.allocPrint(allocator,
            \\{{"importerUri":"{s}","importString":"{s}","projectRoot":"{s}"}}
        , .{ request.importer_uri, request.import_string, request.project_root }) catch return null;
        defer allocator.free(request_json_slice);

        const request_json = allocator.dupeZ(u8, request_json_slice) catch return null;
        defer allocator.free(request_json);

        const response = resolve_fn(request_json.ptr);
        if (response == null) return null;
        defer if (self.free_fn) |free| free(response.?);

        const response_str = std.mem.span(response.?);
        return self.parseResponse(allocator, response_str);
    }

    fn parseResponse(self: *Self, allocator: std.mem.Allocator, json_str: []const u8) ?ResolveResult {
        _ = self;

        const parsed = std.json.parseFromSlice(std.json.Value, allocator, json_str, .{}) catch return null;
        defer parsed.deinit();

        if (parsed.value != .object) return null;
        const obj = parsed.value.object;

        const canonical_id = obj.get("canonicalId") orelse return null;
        if (canonical_id != .string) return null;

        var result = ResolveResult{
            .canonical_id = allocator.dupe(u8, canonical_id.string) catch return null,
        };

        if (obj.get("uri")) |uri_val| {
            if (uri_val == .string) {
                result.uri = allocator.dupe(u8, uri_val.string) catch null;
            }
        }

        if (obj.get("kind")) |kind_val| {
            if (kind_val == .string) {
                if (std.mem.eql(u8, kind_val.string, "file")) {
                    result.kind = .file;
                } else if (std.mem.eql(u8, kind_val.string, "virtual")) {
                    result.kind = .virtual;
                } else if (std.mem.eql(u8, kind_val.string, "remote")) {
                    result.kind = .remote;
                }
            }
        }

        return result;
    }
};

fn uriToPath(uri: []const u8) []const u8 {
    const prefix = "file://";
    if (std.mem.startsWith(u8, uri, prefix)) {
        return uri[prefix.len..];
    }
    return uri;
}
