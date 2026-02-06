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

pub const ResolvedRef = Resolver.ResolvedRef;
pub const ImportSymbolInfo = Resolver.ImportSymbolInfo;
pub const ClassMethodInfo = Resolver.ClassMethodInfo;

/// Export information for a symbol defined in this module.
/// Note: `methods` is owned by this Document and freed in deinit.
pub const ExportInfo = struct {
    kind: Scope.Symbol.Kind,
    inferred_type: ?Scope.Symbol.InferredType = null,
    fn_arity: ?usize = null,
    class_name: ?[]const u8 = null,
    methods: ?[]const ClassMethodInfo = null,
};

module: ast.Module,
bytes: []const u8,
src: [:0]const u8,
language: lsp_namespace.Language,
source_file: *SourceFile,
reporter: Reporter,
symbols: std.ArrayListUnmanaged(SymbolInfo),
refs: std.ArrayListUnmanaged(ResolvedRef) = .empty,
exports: std.StringHashMapUnmanaged(ExportInfo) = .empty,

const log = std.log.scoped(.wren_lsp);

/// Overflow-safe bounds check for tokens.
fn tokenInBounds(t: Token, src_len: usize) bool {
    if (t.length == 0) return false;
    if (t.start >= src_len) return false;
    if (t.length > src_len - t.start) return false;
    return true;
}

pub fn deinit(doc: *Document, gpa: std.mem.Allocator) void {
    doc.reporter.deinit();
    doc.symbols.deinit(gpa);
    doc.refs.deinit(gpa);
    var iter = doc.exports.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.methods) |methods| {
            gpa.free(methods);
        }
    }
    doc.exports.deinit(gpa);
    doc.source_file.deinit();
    gpa.destroy(doc.source_file);
}

pub fn init(
    gpa: std.mem.Allocator,
    src: [:0]const u8,
    language: lsp_namespace.Language,
) !Document {
    return initWithImportSymbols(gpa, src, language, null);
}

pub fn initWithImportSymbols(
    gpa: std.mem.Allocator,
    src: [:0]const u8,
    language: lsp_namespace.Language,
    import_symbols: ?*const std.StringHashMapUnmanaged(ImportSymbolInfo),
) !Document {
    const code_point_iterator = (try unicode.Utf8View.init(src)).iterator();
    const bytes = code_point_iterator.bytes;

    const source_file = try gpa.create(SourceFile);
    source_file.* = try SourceFile.new(gpa, "index.wren", src);

    var reporter = Reporter.init(gpa);

    const lexer = try Lexer.new(gpa, source_file);
    var parser = try Parser.newWithReporter(gpa, lexer, &reporter);
    var module = try parser.parseModule();

    var symbols: std.ArrayListUnmanaged(SymbolInfo) = .empty;
    var refs: std.ArrayListUnmanaged(ResolvedRef) = .empty;
    var exports: std.StringHashMapUnmanaged(ExportInfo) = .empty;

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
            .refs = refs,
            .exports = exports,
        };
    };
    defer resolver.deinit();
    resolver.refs_out = &refs;
    resolver.import_symbols = import_symbols;
    resolver.resolve(&module);

    // Populate symbols from all scopes
    for (resolver.scope.scopes.items) |maybe_scope| {
        if (maybe_scope) |scope| {
            var iter = scope.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;
                if (sym.is_builtin) continue;
                if (!tokenInBounds(sym.token, src.len)) continue;
                symbols.append(gpa, .{
                    .name = sym.name,
                    .token = sym.token,
                    .kind = sym.kind,
                    .inferred_type = sym.inferred_type,
                    .fn_arity = sym.fn_arity,
                    .class_name = sym.class_name,
                }) catch {};
            }
        }
    }

    // Populate exports from module scope (first scope) only
    if (resolver.scope.scopes.items.len > 0) {
        if (resolver.scope.scopes.items[0]) |module_scope| {
            var iter = module_scope.iterator();
            while (iter.next()) |entry| {
                const sym = entry.value_ptr.*;
                if (sym.is_builtin) continue;
                if (!tokenInBounds(sym.token, src.len)) continue;
                // Skip imported symbols (they're not exports of this module)
                if (sym.kind == .import_var) continue;

                const methods: ?[]const ClassMethodInfo = if (sym.kind == .class)
                    extractClassMethods(gpa, &module, sym.name)
                else
                    null;

                exports.put(gpa, sym.name, .{
                    .kind = sym.kind,
                    .inferred_type = sym.inferred_type,
                    .fn_arity = sym.fn_arity,
                    .class_name = sym.class_name,
                    .methods = methods,
                }) catch {
                    if (methods) |m| gpa.free(m);
                };
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
        .refs = refs,
        .exports = exports,
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

fn extractClassMethods(gpa: std.mem.Allocator, module: *const ast.Module, class_name: []const u8) ?[]const ClassMethodInfo {
    for (module.statements) |stmt| {
        switch (stmt) {
            .ClassStmt => |class_stmt| {
                if (class_stmt.name) |name_token| {
                    if (std.mem.eql(u8, name_token.name(), class_name)) {
                        return Resolver.extractMethodsFromAst(gpa, class_stmt.methods);
                    }
                }
            },
            else => {},
        }
    }
    return null;
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

pub fn tokenAtPosition(self: *const Document, line: u32, col: u32) ?Token {
    const offset = self.positionToOffset(line, col) orelse return null;

    for (self.symbols.items) |sym| {
        const start = sym.token.start;
        const end = start + sym.token.length;
        if (offset >= start and offset < end) {
            return sym.token;
        }
    }

    return null;
}

pub fn resolvedAtPosition(self: *const Document, line: u32, col: u32) ?ResolvedRef {
    const offset = self.positionToOffset(line, col) orelse return null;

    // Check if cursor is on a use of a variable
    for (self.refs.items) |ref| {
        const start = ref.use_token.start;
        const end = start + ref.use_token.length;
        if (offset >= start and offset < end) {
            return ref;
        }
    }

    // Check if cursor is on a declaration (decl_token)
    // Return any ref that points to this declaration
    for (self.refs.items) |ref| {
        const start = ref.decl_token.start;
        const end = start + ref.decl_token.length;
        if (offset >= start and offset < end) {
            return ref;
        }
    }

    return null;
}

test "Document exports populated for classes and variables" {
    const src =
        \\class MyClass {}
        \\var myNum = 1
        \\var myFn = Fn.new { |x| x }
    ;

    var doc = try Document.init(std.testing.allocator, src, .wren);
    defer doc.deinit(std.testing.allocator);

    // Check that exports are populated
    try std.testing.expectEqual(@as(usize, 3), doc.exports.count());

    // Check class export
    const class_export = doc.exports.get("MyClass");
    try std.testing.expect(class_export != null);
    try std.testing.expectEqual(Scope.Symbol.Kind.class, class_export.?.kind);

    // Check num variable export
    const num_export = doc.exports.get("myNum");
    try std.testing.expect(num_export != null);
    try std.testing.expectEqual(Scope.Symbol.Kind.variable, num_export.?.kind);
    try std.testing.expectEqual(Scope.Symbol.InferredType.num, num_export.?.inferred_type.?);

    // Check fn variable export
    const fn_export = doc.exports.get("myFn");
    try std.testing.expect(fn_export != null);
    try std.testing.expectEqual(Scope.Symbol.Kind.variable, fn_export.?.kind);
    try std.testing.expectEqual(Scope.Symbol.InferredType.fn_type, fn_export.?.inferred_type.?);
    try std.testing.expectEqual(@as(usize, 1), fn_export.?.fn_arity.?);
}

test "Document with import symbols uses provided type info" {
    const src =
        \\import "other" for fn, a
        \\fn.call()
    ;

    // Simulate import symbols from another module
    var import_symbols: std.StringHashMapUnmanaged(ImportSymbolInfo) = .empty;
    defer import_symbols.deinit(std.testing.allocator);

    try import_symbols.put(std.testing.allocator, "fn", .{
        .kind = .variable,
        .inferred_type = .fn_type,
        .fn_arity = 1,
    });
    try import_symbols.put(std.testing.allocator, "a", .{
        .kind = .variable,
        .inferred_type = .num,
    });

    var doc = try Document.initWithImportSymbols(std.testing.allocator, src, .wren, &import_symbols);
    defer doc.deinit(std.testing.allocator);

    // Check that imported symbols have correct types
    var fn_found = false;
    var a_found = false;
    for (doc.symbols.items) |sym| {
        if (std.mem.eql(u8, sym.name, "fn")) {
            fn_found = true;
            try std.testing.expectEqual(Scope.Symbol.Kind.variable, sym.kind);
            try std.testing.expectEqual(Scope.Symbol.InferredType.fn_type, sym.inferred_type.?);
            try std.testing.expectEqual(@as(usize, 1), sym.fn_arity.?);
        }
        if (std.mem.eql(u8, sym.name, "a")) {
            a_found = true;
            try std.testing.expectEqual(Scope.Symbol.Kind.variable, sym.kind);
            try std.testing.expectEqual(Scope.Symbol.InferredType.num, sym.inferred_type.?);
        }
    }
    try std.testing.expect(fn_found);
    try std.testing.expect(a_found);

    // Check that arity error is reported (fn.call() has 0 args, fn expects 1)
    const diags = doc.getDiagnostics();
    try std.testing.expect(diags.len > 0);
    // The error should mention argument count mismatch
    var found_arity_error = false;
    for (diags) |diag| {
        if (std.mem.indexOf(u8, diag.message, "argument") != null) {
            found_arity_error = true;
            break;
        }
    }
    try std.testing.expect(found_arity_error);
}
