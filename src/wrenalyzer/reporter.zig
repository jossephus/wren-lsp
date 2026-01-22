//! Reporter - unified error and diagnostic collection for wrenalyzer.
//!
//! Collects parse and semantic errors during analysis and converts them
//! to LSP-compatible diagnostics.

const std = @import("std");
const Token = @import("token.zig").Token;
const SourceFile = @import("source_file.zig");

pub const Reporter = @This();

pub const Severity = enum {
    @"error",
    warning,
    info,
    hint,
};

pub const Diagnostic = struct {
    message: []const u8,
    token: Token,
    severity: Severity,
    related_tokens: []const Token,

    pub fn getLineStart(self: Diagnostic) u32 {
        const line = self.token.source.lineAt(self.token.start);
        return if (line > 0) @intCast(line - 1) else 0;
    }

    pub fn getColumnStart(self: Diagnostic) u32 {
        return @intCast(self.token.source.columnAt(self.token.start));
    }

    pub fn getColumnEnd(self: Diagnostic) u32 {
        const end_offset = self.token.start + self.token.length;
        return @intCast(self.token.source.columnAt(end_offset));
    }
};

allocator: std.mem.Allocator,
diagnostics: std.ArrayListUnmanaged(Diagnostic),

pub fn init(allocator: std.mem.Allocator) Reporter {
    return .{
        .allocator = allocator,
        .diagnostics = .empty,
    };
}

pub fn deinit(self: *Reporter) void {
    self.diagnostics.deinit(self.allocator);
}

pub fn reportError(self: *Reporter, token: Token, message: []const u8) void {
    self.report(token, message, .@"error", &.{});
}

pub fn reportWarning(self: *Reporter, token: Token, message: []const u8) void {
    self.report(token, message, .warning, &.{});
}

pub fn report(
    self: *Reporter,
    token: Token,
    message: []const u8,
    severity: Severity,
    related: []const Token,
) void {
    self.diagnostics.append(self.allocator, .{
        .message = message,
        .token = token,
        .severity = severity,
        .related_tokens = related,
    }) catch {
        std.log.err("Failed to append diagnostic", .{});
    };
}

pub fn hasErrors(self: *const Reporter) bool {
    for (self.diagnostics.items) |diag| {
        if (diag.severity == .@"error") return true;
    }
    return false;
}

pub fn errorCount(self: *const Reporter) usize {
    var count: usize = 0;
    for (self.diagnostics.items) |diag| {
        if (diag.severity == .@"error") count += 1;
    }
    return count;
}

pub fn clear(self: *Reporter) void {
    self.diagnostics.clearRetainingCapacity();
}
