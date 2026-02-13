//! Cross-platform file URI utilities.
//!
//! Centralizes conversion between file:// URIs and filesystem paths,
//! handling Windows drive-letter URIs (file:///C:/path) and backslash
//! normalization.

const std = @import("std");
const builtin = @import("builtin");

/// Convert a file:// URI to a filesystem path.
///
/// On Windows, strips the leading `/` from drive-letter paths
/// (e.g. `file:///C:/dir/file` → `C:/dir/file`).
pub fn uriToPath(uri: []const u8) []const u8 {
    const prefix = "file://";
    if (std.mem.startsWith(u8, uri, prefix)) {
        const path = uri[prefix.len..];
        // On Windows, file:///C:/path becomes /C:/path after stripping prefix.
        // Remove the leading / so we get a valid drive-letter path.
        if (builtin.os.tag == .windows and path.len > 2 and path[0] == '/' and path[2] == ':') {
            return path[1..];
        }
        return path;
    }
    return uri;
}

/// Construct a file:// URI from a filesystem path.
///
/// On Windows, converts backslashes to forward slashes and ensures
/// the URI has the form `file:///C:/path`. On Unix, paths already
/// start with `/` so the result is `file:///path`.
pub fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    if (builtin.os.tag == .windows) {
        // Normalize backslashes to forward slashes for the URI
        const normalized = try std.mem.replaceOwned(u8, allocator, path, "\\", "/");
        defer allocator.free(normalized);
        // Windows absolute paths like C:/... need an extra leading /
        if (normalized.len >= 2 and normalized[1] == ':') {
            return std.fmt.allocPrint(allocator, "file:///{s}", .{normalized});
        }
        return std.fmt.allocPrint(allocator, "file://{s}", .{normalized});
    }
    // Unix: absolute paths start with /, so file:// + /path = file:///path
    return std.fmt.allocPrint(allocator, "file://{s}", .{path});
}
