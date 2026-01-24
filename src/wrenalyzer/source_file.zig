const std = @import("std");
const unicode = std.unicode;
const Chars = @import("chars.zig").Chars;

pub const SourceFile = @This();

pub const log = std.log.scoped(.wren_lsp);

path: []const u8,
code: []const u8,
bytes: []const u8,
lines: []usize,
allocator: std.mem.Allocator,

pub fn new(allocator: std.mem.Allocator, path: []const u8, code: []const u8) !SourceFile {
    const code_point_iterator = (try unicode.Utf8View.init(code)).iterator();

    const bytes = code_point_iterator.bytes;

    return .{
        .path = path,
        .code = code,
        .bytes = bytes,
        .lines = try findLines(allocator, bytes),
        .allocator = allocator,
    };
}

pub fn deinit(self: *SourceFile) void {
    self.allocator.free(self.lines);
}

pub fn columnAt(self: SourceFile, offset: usize) usize {
    var column: usize = 1;

    var i = offset - 1;
    while (i >= 0) {
        if (self.bytes[i] == Chars.lineFeed.int()) break;
        column = column + 1;
        i = i - 1;
    }

    return column;
}

pub fn lineAt(self: SourceFile, offset: usize) usize {
    for (self.lines, 0..) |_, i| {
        if (offset < self.lines[i]) return i;
    }
    return self.lines.len;
}

fn findLines(allocator: std.mem.Allocator, bytes: []const u8) ![]usize {
    var lines: std.ArrayListUnmanaged(usize) = .empty;
    defer lines.deinit(allocator);

    try lines.append(allocator, 0);

    for (bytes, 0..) |b, i| {
        if (b == '\n') {
            try lines.append(allocator, i + 1);
        }
    }

    return lines.toOwnedSlice(allocator);
}
