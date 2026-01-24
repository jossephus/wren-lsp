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
const Scope = wrenalyzer.Scope;
const Token = wrenalyzer.Token;

pub const SymbolInfo = struct {
    name: []const u8,
    token: Token,
    kind: Scope.Symbol.Kind,
    inferred_type: ?Scope.Symbol.InferredType,
    fn_arity: ?usize,
    class_name: ?[]const u8,
};

module: ast.Module,
bytes: []const u8,
src: []const u8,
language: lsp_namespace.Language,
source_file: SourceFile,
reporter: Reporter,
symbols: std.ArrayListUnmanaged(SymbolInfo),

const log = std.log.scoped(.wren_lsp);

pub fn deinit(doc: *Document, gpa: std.mem.Allocator) void {
    doc.reporter.deinit();
    doc.symbols.deinit(gpa);
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

    var symbols: std.ArrayListUnmanaged(SymbolInfo) = .empty;

    var resolver = Resolver.init(gpa, &reporter) catch {
        log.err("Failed to init resolver", .{});
        return .{
            .src = src,
            .language = language,
            .bytes = bytes,
            .source_file = source_file,
            .module = module,
            .reporter = reporter,
            .symbols = symbols,
        };
    };
    defer resolver.deinit();
    resolver.resolve(&module);

    for (resolver.scope.scopes.items) |maybe_scope| {
        if (maybe_scope) |scope| {
            var iter = scope.iterator();
            while (iter.next()) |entry| {
                symbols.append(gpa, .{
                    .name = entry.value_ptr.name,
                    .token = entry.value_ptr.token,
                    .kind = entry.value_ptr.kind,
                    .inferred_type = entry.value_ptr.inferred_type,
                    .fn_arity = entry.value_ptr.fn_arity,
                    .class_name = entry.value_ptr.class_name,
                }) catch {};
            }
        }
    }

    return .{
        .src = src,
        .language = language,
        .bytes = bytes,
        .source_file = source_file,
        .module = module,
        .reporter = reporter,
        .symbols = symbols,
    };
}

pub fn hasErrors(self: *const Document) bool {
    return self.reporter.hasErrors();
}

pub fn getDiagnostics(self: *const Document) []const Reporter.Diagnostic {
    return self.reporter.diagnostics.items;
}

pub fn positionToOffset(self: *const Document, line: u32, character: u32) ?usize {
    const lines = self.source_file.lines;
    if (line >= lines.len) return null;

    const line_start = lines[line];
    const line_end = if (line + 1 < lines.len) lines[line + 1] else self.src.len;

    var offset = line_start;
    var col: u32 = 0;
    while (offset < line_end and col < character) {
        if (offset < self.src.len and self.src[offset] == '\n') break;
        offset += 1;
        col += 1;
    }

    return offset;
}

pub fn findSymbolAtPosition(self: *const Document, line: u32, character: u32) ?SymbolInfo {
    const offset = self.positionToOffset(line, character) orelse return null;

    for (self.symbols.items) |sym| {
        const start = sym.token.start;
        const end = start + sym.token.length;
        if (offset >= start and offset < end) {
            return sym;
        }
    }

    return null;
}

pub fn getSymbolsInScope(self: *const Document) []const SymbolInfo {
    return self.symbols.items;
}
