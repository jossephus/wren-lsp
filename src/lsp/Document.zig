const std = @import("std");
const Document = @This();
const lsp_namespace = @import("../lsp.zig");
const unicode = std.unicode;

//arena: std.heap.ArenaAllocator,
//bytes: [:0]const u8,
bytes: []const u8,
src: []const u8,
language: lsp_namespace.Language,

pub fn deinit(doc: *Document, gpa: std.mem.Allocator) void {
    _ = doc;
    _ = gpa;
    //gpa.deinit();
}

pub fn init(
    gpa: std.mem.Allocator,
    src: []const u8,
    language: lsp_namespace.Language,
) !Document {
    const code_point_iterator = (try unicode.Utf8View.init(src)).iterator();

    const bytes = code_point_iterator.bytes;

    var doc: Document = .{
        .src = src,
        .language = language,
        .bytes = bytes,
    };
    errdefer doc.html.deinit(gpa);

    return doc;
}
