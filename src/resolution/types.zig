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
    /// Resolver-provided completions.
    completions: []const []const u8 = &.{},

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
    /// Canonical module id of the importer.
    importer_module_id: []const u8,
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

/// Diagnostics configuration.
pub const DiagnosticsConfig = struct {
    missing_import: DiagnosticSeverity = .warning,
    extension_in_import: DiagnosticSeverity = .info,
    unknown_variable: DiagnosticSeverity = .@"error",
};

/// Resolver type discriminator.
pub const ResolverKind = enum {
    path,
    plugin,

    pub fn fromString(s: []const u8) ?ResolverKind {
        const map = std.StaticStringMap(ResolverKind).initComptime(.{
            .{ "path", .path },
            .{ "plugin", .plugin },
        });
        return map.get(s);
    }
};

/// Path resolver configuration.
pub const PathResolverConfig = struct {
    roots: []const []const u8 = &.{},
    delimiter: []const u8 = "/",
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
    plugin: PluginResolverConfig = .{ .library = "" },
};

/// Top-level wren-lsp.json configuration.
pub const Config = struct {
    version: u32 = 1,
    /// Path to parent config to inherit from.
    extends: ?[]const u8 = null,
    /// Directories to index for Wren files.
    modules: []const []const u8 = &.{},
    /// Ordered list of resolvers.
    resolvers: []const ResolverConfig = &.{},
    /// Diagnostic severity settings.
    diagnostics: DiagnosticsConfig = .{},

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
