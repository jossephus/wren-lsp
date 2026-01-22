//! Wren LSP Server implementation using lsp-kit

const std = @import("std");
const builtin = @import("builtin");

const lsp = @import("lsp");
const offsets = lsp.offsets;
const types = lsp.types;
const wrenalyzer = @import("wrenalyzer");
const Token = wrenalyzer.Token;
const Scope = wrenalyzer.Scope;

pub const Language = enum {
    wren,
    pub fn fromSliceResilient(s: []const u8) ?Language {
        const Alias = enum { wren };

        const alias = std.meta.stringToEnum(Alias, s) orelse {
            return null;
        };

        return switch (alias) {
            .wren => .wren,
        };
    }
};

const Document = @import("lsp/Document.zig");

pub const log = std.log.scoped(.wren_lsp);

pub const Handler = struct {
    gpa: std.mem.Allocator,
    transport: *lsp.Transport,
    files: std.StringHashMapUnmanaged(Document) = .{},
    offset_encoding: offsets.Encoding = .@"utf-16",

    pub fn init(gpa: std.mem.Allocator, transport: *lsp.Transport) Handler {
        return .{
            .gpa = gpa,
            .transport = transport,
        };
    }

    pub fn initialize(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.InitializeParams,
    ) !types.InitializeResult {
        _ = arena;

        if (params.capabilities.general) |general| {
            if (general.positionEncodings) |encodings| {
                for (encodings) |enc| {
                    if (enc == .@"utf-8") {
                        self.offset_encoding = .@"utf-8";
                        break;
                    }
                }
            }
        }

        if (params.clientInfo) |clientInfo| {
            log.info("client is '{s}-{s}'", .{ clientInfo.name, clientInfo.version orelse "<no version>" });
        }

        log.debug("init!", .{});

        return .{
            .serverInfo = .{
                .name = "Wren LSP",
                .version = "0.0.1",
            },
            .capabilities = .{
                .positionEncoding = switch (self.offset_encoding) {
                    .@"utf-8" => .@"utf-8",
                    .@"utf-16" => .@"utf-16",
                    .@"utf-32" => .@"utf-32",
                },
                .textDocumentSync = .{
                    .TextDocumentSyncOptions = .{
                        .openClose = true,
                        .change = .Full,
                        .save = .{ .bool = true },
                    },
                },
                .completionProvider = .{
                    .triggerCharacters = &[_][]const u8{"."},
                },
                .hoverProvider = .{ .bool = true },
                .definitionProvider = .{ .bool = true },
                .referencesProvider = .{ .bool = false },
                .documentFormattingProvider = .{ .bool = false },
                .semanticTokensProvider = .{
                    .SemanticTokensOptions = .{
                        .full = .{ .bool = true },
                        .legend = .{
                            .tokenTypes = std.meta.fieldNames(types.SemanticTokenTypes),
                            .tokenModifiers = std.meta.fieldNames(types.SemanticTokenModifiers),
                        },
                    },
                },
                .inlayHintProvider = .{ .bool = false },
            },
        };
    }

    pub fn initialized(
        _: *Handler,
        _: std.mem.Allocator,
        _: types.InitializedParams,
    ) !void {}

    pub fn shutdown(
        _: *Handler,
        _: std.mem.Allocator,
        _: void,
    ) !?void {
        return null;
    }

    pub fn @"textDocument/didOpen"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.DidOpenTextDocumentParams,
    ) !void {
        log.debug("Opening document", .{});
        const new_text = try self.gpa.dupeZ(u8, params.textDocument.text);
        errdefer self.gpa.free(new_text);

        const language_id = params.textDocument.languageId;
        const language = Language.fromSliceResilient(language_id) orelse {
            log.err("unrecognized language id: '{s}'", .{language_id});
            return;
        };
        try self.loadFile(
            arena,
            new_text,
            params.textDocument.uri,
            language,
        );
    }

    pub fn @"textDocument/didChange"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.DidChangeTextDocumentParams,
    ) !void {
        if (params.contentChanges.len == 0) {
            return;
        }

        const file = self.files.get(params.textDocument.uri) orelse {
            log.err("changeDocument failed: unknown file", .{});
            return;
        };

        for (params.contentChanges) |change_| {
            const new_text = switch (change_) {
                .literal_1 => |change| try self.gpa.dupeZ(u8, change.text),
                .literal_0 => |change| blk: {
                    const old_text = file.src;
                    const range = change.range;
                    const start_idx = offsets.positionToIndex(old_text, range.start, self.offset_encoding);
                    const end_idx = offsets.positionToIndex(old_text, range.end, self.offset_encoding);
                    var new_text_list: std.ArrayListUnmanaged(u8) = .empty;
                    errdefer new_text_list.deinit(self.gpa);
                    try new_text_list.appendSlice(self.gpa, old_text[0..start_idx]);
                    try new_text_list.appendSlice(self.gpa, change.text);
                    try new_text_list.appendSlice(self.gpa, old_text[end_idx..]);
                    break :blk try new_text_list.toOwnedSliceSentinel(self.gpa, 0);
                },
            };
            errdefer self.gpa.free(new_text);

            try self.loadFile(
                arena,
                new_text,
                params.textDocument.uri,
                file.language,
            );
        }
    }

    pub fn @"textDocument/didSave"(
        _: *Handler,
        _: std.mem.Allocator,
        _: types.DidSaveTextDocumentParams,
    ) !void {}

    pub fn @"textDocument/didClose"(
        self: *Handler,
        _: std.mem.Allocator,
        params: types.DidCloseTextDocumentParams,
    ) !void {
        var kv = self.files.fetchRemove(params.textDocument.uri) orelse return;
        self.gpa.free(kv.key);
        kv.value.deinit(self.gpa);
    }

    pub fn @"textDocument/completion"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.CompletionParams,
    ) !lsp.ResultType("textDocument/completion") {
        const doc = self.files.get(params.textDocument.uri) orelse {
            return .{ .CompletionList = .{ .isIncomplete = false, .items = &.{} } };
        };

        const symbols = doc.getSymbolsInScope();
        var items = try arena.alloc(types.CompletionItem, symbols.len);

        for (symbols, 0..) |sym, i| {
            items[i] = .{
                .label = sym.name,
                .kind = symbolKindToCompletionKind(sym.kind),
            };
        }

        return .{
            .CompletionList = .{
                .isIncomplete = false,
                .items = items,
            },
        };
    }

    pub fn @"textDocument/hover"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.HoverParams,
    ) !?types.Hover {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        const sym = doc.findSymbolAtPosition(
            params.position.line,
            params.position.character,
        ) orelse return null;

        const kind_str = switch (sym.kind) {
            .variable => "variable",
            .parameter => "parameter",
            .class => "class",
            .method => "method",
            .field => "field",
            .static_field => "static field",
            .import_var => "import",
        };

        const content = try std.fmt.allocPrint(arena, "**{s}** `{s}`", .{ kind_str, sym.name });

        return .{
            .contents = .{ .MarkupContent = .{ .kind = .markdown, .value = content } },
            .range = tokenToRange(sym.token),
        };
    }

    pub fn @"textDocument/definition"(
        self: *Handler,
        _: std.mem.Allocator,
        params: types.DefinitionParams,
    ) !lsp.ResultType("textDocument/definition") {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        const sym = doc.findSymbolAtPosition(
            params.position.line,
            params.position.character,
        ) orelse return null;

        return .{
            .Definition = .{
                .Location = .{
                    .uri = params.textDocument.uri,
                    .range = tokenToRange(sym.token),
                },
            },
        };
    }

    pub fn @"textDocument/references"(
        _: *Handler,
        _: std.mem.Allocator,
        _: types.ReferenceParams,
    ) !lsp.ResultType("textDocument/references") {
        return null;
    }

    pub fn @"textDocument/semanticTokens/full"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.SemanticTokensParams,
    ) !?types.SemanticTokens {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        var data: std.ArrayListUnmanaged(u32) = .empty;
        var prev_line: u32 = 0;
        var prev_col: u32 = 0;

        var lexer = try wrenalyzer.Lexer.new(arena, doc.source_file);

        while (true) {
            const token = try lexer.readToken();
            if (token.type == .eof) break;

            const token_type = tokenTagToSemanticType(token.type) orelse continue;

            const line_num = doc.source_file.lineAt(token.start);
            const line: u32 = if (line_num > 0) @intCast(line_num - 1) else 0;
            const col_num = doc.source_file.columnAt(token.start);
            const col: u32 = if (col_num > 0) @intCast(col_num - 1) else 0;

            const delta_line = line - prev_line;
            const delta_col = if (delta_line == 0) col - prev_col else col;

            try data.append(arena, delta_line);
            try data.append(arena, delta_col);
            try data.append(arena, @intCast(token.length));
            try data.append(arena, @intFromEnum(token_type));
            try data.append(arena, 0);

            prev_line = line;
            prev_col = col;
        }

        return .{ .data = data.items };
    }

    pub fn @"textDocument/inlayHint"(
        _: *Handler,
        _: std.mem.Allocator,
        _: types.InlayHintParams,
    ) !lsp.ResultType("textDocument/inlayHint") {
        return null;
    }

    pub fn onResponse(
        _: *Handler,
        _: std.mem.Allocator,
        _: lsp.JsonRPCMessage.Response,
    ) !void {}

    fn loadFile(
        self: *Handler,
        arena: std.mem.Allocator,
        new_text: [:0]const u8,
        uri: []const u8,
        language: Language,
    ) !void {
        var res: types.PublishDiagnosticsParams = .{
            .uri = uri,
            .diagnostics = &.{},
        };

        const doc = try Document.init(
            self.gpa,
            new_text,
            language,
        );

        log.debug("document init", .{});

        const gop = try self.files.getOrPut(self.gpa, uri);
        errdefer _ = self.files.remove(uri);

        if (gop.found_existing) {
            gop.value_ptr.deinit(self.gpa);
        } else {
            gop.key_ptr.* = try self.gpa.dupe(u8, uri);
        }

        gop.value_ptr.* = doc;

        const reporter_diags = doc.getDiagnostics();
        log.debug("document errors: {d}", .{reporter_diags.len});

        if (reporter_diags.len != 0) {
            const diags = try arena.alloc(types.Diagnostic, reporter_diags.len);

            for (reporter_diags, diags) |rdiag, *d| {
                log.debug("Error is {s}", .{rdiag.message});
                d.* = .{
                    .range = tokenToRange(rdiag.token),
                    .severity = switch (rdiag.severity) {
                        .@"error" => .Error,
                        .warning => .Warning,
                        .info => .Information,
                        .hint => .Hint,
                    },
                    .message = rdiag.message,
                };
            }

            res.diagnostics = diags;
        }

        try self.transport.writeNotification(
            self.gpa,
            "textDocument/publishDiagnostics",
            types.PublishDiagnosticsParams,
            res,
            .{ .emit_null_optional_fields = false },
        );
    }
};

pub fn tokenToRange(token: Token) types.Range {
    const line_num = token.source.lineAt(token.start);
    const line: u32 = if (line_num > 0) @intCast(line_num - 1) else 0;

    const col_start = token.source.columnAt(token.start);
    const col_end = token.source.columnAt(token.start + token.length);

    return .{
        .start = .{
            .line = line,
            .character = if (col_start > 0) @intCast(col_start - 1) else 0,
        },
        .end = .{
            .line = line,
            .character = if (col_end > 0) @intCast(col_end - 1) else 0,
        },
    };
}

fn symbolKindToCompletionKind(kind: Scope.Symbol.Kind) types.CompletionItemKind {
    return switch (kind) {
        .variable => .Variable,
        .parameter => .Variable,
        .class => .Class,
        .method => .Method,
        .field => .Field,
        .static_field => .Field,
        .import_var => .Module,
    };
}

fn tokenTagToSemanticType(tag: Token.Tag) ?types.SemanticTokenTypes {
    return switch (tag) {
        .breakKeyword,
        .classKeyword,
        .constructKeyword,
        .elseKeyword,
        .falseKeyword,
        .forKeyword,
        .foreignKeyword,
        .ifKeyword,
        .importKeyword,
        .inKeyword,
        .isKeyword,
        .nullKeyword,
        .returnKeyword,
        .staticKeyword,
        .superKeyword,
        .thisKeyword,
        .trueKeyword,
        .varKeyword,
        .whileKeyword,
        => .keyword,
        .name => .variable,
        .number => .number,
        .string, .interpolation => .string,
        .field, .staticField => .property,
        else => null,
    };
}
