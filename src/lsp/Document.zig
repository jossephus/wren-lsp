const std = @import("std");
const Document = @This();
const lsp_namespace = @import("../lsp.zig");
const unicode = std.unicode;
const wrenalyzer = @import("wrenalyzer");
const ast = wrenalyzer.Ast;
const Parser = wrenalyzer.Parser;
const SourceFile = wrenalyzer.SourceFile;
const Lexer = wrenalyzer.Lexer;

//arena: std.heap.ArenaAllocator,
//bytes: [:0]const u8,
module: ast.Module,
bytes: []const u8,
src: []const u8,
language: lsp_namespace.Language,
parser: Parser,

pub fn deinit(doc: *Document, gpa: std.mem.Allocator) void {
    _ = doc;
    _ = gpa;
    //gpa.deinit();
}

const log = std.log.scoped(.wren_lsp);

pub fn init(
    gpa: std.mem.Allocator,
    src: []const u8,
    language: lsp_namespace.Language,
) !Document {
    const code_point_iterator = (try unicode.Utf8View.init(src)).iterator();

    const bytes = code_point_iterator.bytes;

    const source_file = try SourceFile.new(
        gpa,
        "index.wren",
        src,
    );
    const lexer = try Lexer.new(gpa, source_file);
    var parser = try Parser.new(gpa, lexer);
    const module = try parser.parseModule();

    const doc: Document = .{
        .src = src,
        .language = language,
        .bytes = bytes,
        .parser = parser,
        .module = module,
    };

    return doc;
}
