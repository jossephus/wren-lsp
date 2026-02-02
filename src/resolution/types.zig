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
    /// Module source content. If provided, used directly instead of reading from URI.
    source: ?[]const u8 = null,
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

/// Module entry - either a directory path or a named module mapping.
pub const ModuleEntry = union(enum) {
    /// Directory path to scan for .wren files.
    directory: []const u8,
    /// Named module: maps a module name to a specific file path.
    named: NamedModule,

    pub const NamedModule = struct {
        name: []const u8,
        path: []const u8,
    };

    /// Get the path for this entry (directory path or named module's file path).
    pub fn getPath(self: ModuleEntry) []const u8 {
        return switch (self) {
            .directory => |d| d,
            .named => |n| n.path,
        };
    }

    /// Extract directory paths from a slice of module entries.
    pub fn extractDirectories(allocator: std.mem.Allocator, entries: []const ModuleEntry) ![]const []const u8 {
        var dirs: std.ArrayListUnmanaged([]const u8) = .empty;
        for (entries) |entry| {
            switch (entry) {
                .directory => |d| try dirs.append(allocator, d),
                .named => {},
            }
        }
        return dirs.toOwnedSlice(allocator);
    }
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
    /// Paths to parent configs to inherit from (later entries override earlier).
    extends: []const []const u8 = &.{},
    /// Module entries: directories to scan or named module mappings.
    modules: []const ModuleEntry = &.{},
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
