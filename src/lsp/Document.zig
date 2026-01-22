//! Document - represents a parsed Wren source file with analysis results.

const std = @import("std");
const Document = @This();
const lsp_namespace = @import("../lsp.zig");
const unicode = std.unicode;
const wrenalyzer = @import("wrenalyzer");
const ast = wrenalyzer.Ast;
const Parser = wrenalyzer.Parser;
const SourceFile = wrenalyzer.SourceFile;
const Lexer = wrenalyzer.Lexer;
const Reporter = wrenalyzer.Reporter;
const Resolver = wrenalyzer.Resolver;

module: ast.Module,
bytes: []const u8,
src: []const u8,
language: lsp_namespace.Language,
source_file: SourceFile,
reporter: Reporter,

const log = std.log.scoped(.wren_lsp);

pub fn deinit(doc: *Document, gpa: std.mem.Allocator) void {
    doc.reporter.deinit();
    _ = gpa;
}

pub fn init(
    gpa: std.mem.Allocator,
    src: []const u8,
    language: lsp_namespace.Language,
) !Document {
    const code_point_iterator = (try unicode.Utf8View.init(src)).iterator();
    const bytes = code_point_iterator.bytes;

    const source_file = try SourceFile.new(gpa, "index.wren", src);

    var reporter = Reporter.init(gpa);

    const lexer = try Lexer.new(gpa, source_file);
    var parser = try Parser.newWithReporter(gpa, lexer, &reporter);
    var module = try parser.parseModule();

    if (!reporter.hasErrors()) {
        var resolver = try Resolver.init(gpa, &reporter);
        defer resolver.deinit();
        resolver.resolve(&module);
    }

    return .{
        .src = src,
        .language = language,
        .bytes = bytes,
        .source_file = source_file,
        .module = module,
        .reporter = reporter,
    };
}

pub fn hasErrors(self: *const Document) bool {
    return self.reporter.hasErrors();
}

pub fn getDiagnostics(self: *const Document) []const Reporter.Diagnostic {
    return self.reporter.diagnostics.items;
}
