//! Module resolution system for wren-lsp.
//!
//! Provides a configurable resolver chain that maps import strings to
//! canonical module identifiers and file URIs.

pub const types = @import("types.zig");
pub const config = @import("config.zig");
pub const resolver = @import("resolver.zig");

pub const Config = types.Config;
pub const ConfigLoader = config.ConfigLoader;
pub const ResolverChain = resolver.ResolverChain;
pub const ResolveRequest = types.ResolveRequest;
pub const ResolveResult = types.ResolveResult;
pub const DiagnosticsConfig = types.DiagnosticsConfig;
pub const DiagnosticSeverity = types.DiagnosticSeverity;
pub const ModuleEntry = types.ModuleEntry;

test {
    _ = @import("test_resolution.zig");
    _ = config;
}
