//! Core types for module resolution.
//!
//! These types mirror the JSON schema from wren-lsp.json and provide
//! the interface for resolver implementations.

pub const std = @import("std");

/// Result of resolving an import string.
pub const ResolveResult = struct {
    /// Unique identifier for the module (file:// or wren:// URI).
    canonical_id: []const u8,
    /// File path if the module is file-backed, null for virtual modules.
    uri: ?[]const u8 = null,
    /// Module kind for diagnostics policy.
    kind: ModuleKind = .file,
    /// Resolver-provided diagnostics.
    diagnostics: []const Diagnostic = &.{},

    pub const ModuleKind = enum {
        file,
        virtual,
        remote,
    };

    pub const Diagnostic = struct {
        severity: Severity,
        message: []const u8,

        pub const Severity = enum {
            @"error",
            warning,
            info,
            hint,
        };
    };
};

/// Request passed to resolvers.
pub const ResolveRequest = struct {
    /// URI of the file containing the import statement.
    importer_uri: []const u8,
    /// The raw import string from the source.
    import_string: []const u8,
    /// Project root directory (where wren-lsp.json is located).
    project_root: []const u8,
    /// Configured module base directories.
    module_bases: []const []const u8 = &.{},
};

/// Diagnostic severity configuration.
pub const DiagnosticSeverity = enum {
    @"error",
    warning,
    info,
    hint,
    none,

    pub fn fromString(s: []const u8) DiagnosticSeverity {
        const map = std.StaticStringMap(DiagnosticSeverity).initComptime(.{
            .{ "error", .@"error" },
            .{ "warning", .warning },
            .{ "info", .info },
            .{ "hint", .hint },
            .{ "none", .none },
        });
        return map.get(s) orelse .warning;
    }
};

/// Diagnostics configuration from imports.diagnostics.
pub const DiagnosticsConfig = struct {
    missing_import: DiagnosticSeverity = .warning,
    unknown_scheme: DiagnosticSeverity = .none,
    extension_in_import: DiagnosticSeverity = .info,
};

/// Workspace configuration.
pub const WorkspaceConfig = struct {
    roots: []const []const u8 = &.{},
    library: []const []const u8 = &.{},
    stubs: []const []const u8 = &.{},
    exclude: []const []const u8 = &.{},
};

/// Resolver type discriminator.
pub const ResolverKind = enum {
    path,
    dotted,
    scheme,
    stubs,
    plugin,

    pub fn fromString(s: []const u8) ?ResolverKind {
        const map = std.StaticStringMap(ResolverKind).initComptime(.{
            .{ "path", .path },
            .{ "dotted", .dotted },
            .{ "scheme", .scheme },
            .{ "stubs", .stubs },
            .{ "plugin", .plugin },
        });
        return map.get(s);
    }
};

/// Path resolver configuration.
pub const PathResolverConfig = struct {
    roots: []const []const u8 = &.{},
    extensions: []const []const u8 = &.{".wren"},
    index_files: []const []const u8 = &.{},
};

/// Dotted resolver configuration.
pub const DottedResolverConfig = struct {
    delimiter: []const u8 = ".",
    map_to: []const u8 = "/",
    roots: []const []const u8 = &.{},
    extensions: []const []const u8 = &.{".wren"},
};

/// Scheme resolver configuration.
pub const SchemeResolverConfig = struct {
    scheme: []const u8,
    strip_prefix: []const u8,
};

/// Plugin resolver configuration.
pub const PluginResolverConfig = struct {
    library: []const u8,
    fallback_to_builtin: bool = true,
};

/// Union of all resolver configurations.
pub const ResolverConfig = struct {
    kind: ResolverKind,
    path: PathResolverConfig = .{},
    dotted: DottedResolverConfig = .{},
    scheme: SchemeResolverConfig = .{ .scheme = "", .strip_prefix = "" },
    plugin: PluginResolverConfig = .{ .library = "" },
};

/// Top-level imports configuration.
pub const ImportsConfig = struct {
    prefer_relative_from_importer: bool = true,
    resolvers: []const ResolverConfig = &.{},
    diagnostics: DiagnosticsConfig = .{},
};

/// Top-level wren-lsp.json configuration.
pub const Config = struct {
    version: u32 = 1,
    workspace: WorkspaceConfig = .{},
    imports: ImportsConfig = .{},

    /// Config file path for reference.
    config_path: ?[]const u8 = null,
    /// Project root directory.
    project_root: ?[]const u8 = null,

    /// Arena that owns all allocations for this config.
    arena: ?*std.heap.ArenaAllocator = null,

    pub fn deinit(self: *Config, backing_allocator: std.mem.Allocator) void {
        if (self.arena) |arena| {
            arena.deinit();
            backing_allocator.destroy(arena);
        }
    }
};
