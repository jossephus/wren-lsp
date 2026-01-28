//! Wren LSP Server implementation using lsp-kit

const std = @import("std");
const builtin = @import("builtin");

const lsp = @import("lsp");
const offsets = lsp.offsets;
const types = lsp.types;
const wrenalyzer = @import("wrenalyzer");
const Token = wrenalyzer.Token;
const Scope = wrenalyzer.Scope;

const resolution = @import("resolution/root.zig");
const ConfigLoader = resolution.ConfigLoader;
const ResolverChain = resolution.ResolverChain;
const ResolveRequest = resolution.ResolveRequest;
const ResolveResult = resolution.ResolveResult;
const DiagnosticSeverity = resolution.DiagnosticSeverity;

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

/// Import edge: which symbols are imported from which module
pub const ImportEdge = struct {
    from_uri: []const u8,
    to_uri: []const u8,
    symbols: []const []const u8,
};

pub const Handler = struct {
    gpa: std.mem.Allocator,
    transport: *lsp.Transport,
    files: std.StringHashMapUnmanaged(Document) = .{},
    offset_encoding: offsets.Encoding = .@"utf-16",
    config_loader: ConfigLoader,
    resolver_chains: std.StringHashMapUnmanaged(ResolverChain) = .empty,
    /// Maps module URI -> list of URIs that import from it
    reverse_imports: std.StringHashMapUnmanaged(std.ArrayListUnmanaged([]const u8)) = .empty,
    /// Maps (importer URI, symbol name) -> source module URI
    symbol_sources: std.StringHashMapUnmanaged([]const u8) = .empty,

    pub fn init(gpa: std.mem.Allocator, transport: *lsp.Transport) Handler {
        return .{
            .gpa = gpa,
            .transport = transport,
            .config_loader = ConfigLoader.init(gpa),
        };
    }

    pub fn deinit(self: *Handler) void {
        var iter = self.resolver_chains.iterator();
        while (iter.next()) |entry| {
            self.gpa.free(entry.key_ptr.*);
            entry.value_ptr.deinit();
        }
        self.resolver_chains.deinit(self.gpa);
        self.config_loader.deinit();

        var rev_iter = self.reverse_imports.iterator();
        while (rev_iter.next()) |entry| {
            for (entry.value_ptr.items) |uri| {
                self.gpa.free(uri);
            }
            entry.value_ptr.deinit(self.gpa);
            self.gpa.free(entry.key_ptr.*);
        }
        self.reverse_imports.deinit(self.gpa);

        var sym_iter = self.symbol_sources.iterator();
        while (sym_iter.next()) |entry| {
            self.gpa.free(entry.key_ptr.*);
            self.gpa.free(entry.value_ptr.*);
        }
        self.symbol_sources.deinit(self.gpa);

        var file_iter = self.files.iterator();
        while (file_iter.next()) |entry| {
            self.gpa.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.gpa);
        }
        self.files.deinit(self.gpa);
    }

    fn getResolverChain(self: *Handler, uri: []const u8) !*ResolverChain {
        const config = try self.config_loader.loadForFile(uri);
        const project_root = config.project_root orelse ".";

        const gop = try self.resolver_chains.getOrPut(self.gpa, project_root);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.gpa.dupe(u8, project_root);
            gop.value_ptr.* = try ResolverChain.init(self.gpa, config);
        }
        return gop.value_ptr;
    }

    fn getConfig(self: *Handler, uri: []const u8) resolution.Config {
        return self.config_loader.loadForFile(uri) catch resolution.Config{};
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
                .referencesProvider = .{ .bool = true },
                .renameProvider = .{ .bool = true },
                .documentSymbolProvider = .{ .bool = true },
                .documentHighlightProvider = .{ .bool = true },
                .signatureHelpProvider = .{
                    .triggerCharacters = &[_][]const u8{ "(", "," },
                },
                .codeActionProvider = .{ .bool = true },
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
                .workspaceSymbolProvider = .{ .bool = true },
                .foldingRangeProvider = .{ .bool = true },
                .selectionRangeProvider = .{ .bool = true },
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

        if (try self.getImportPathCompletions(arena, doc, params.position, params.textDocument.uri)) |items| {
            return .{ .CompletionList = .{ .isIncomplete = false, .items = items } };
        }

        const should_check_members = blk: {
            if (params.context) |ctx| {
                if (ctx.triggerKind == .TriggerCharacter and ctx.triggerCharacter != null) {
                    if (std.mem.eql(u8, ctx.triggerCharacter.?, ".")) break :blk true;
                }
            }

            const offset = doc.positionToOffset(params.position.line, params.position.character) orelse break :blk false;
            if (offset == 0) break :blk false;
            break :blk doc.src[offset - 1] == '.';
        };

        log.debug("completion: check_members={}", .{should_check_members});

        if (should_check_members) {
            if (try self.getMemberCompletions(arena, doc, params.position, params.textDocument.uri)) |items| {
                log.debug("completion: member items={d}", .{items.len});
                return .{ .CompletionList = .{ .isIncomplete = false, .items = items } };
            }
            log.debug("completion: no member completions", .{});
        }

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

    fn getMemberCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        position: types.Position,
        uri: []const u8,
    ) !?[]types.CompletionItem {
        if (position.character == 0) return null;

        const offset = doc.positionToOffset(position.line, position.character - 1) orelse return null;

        var end = offset;
        while (end > 0 and isIdentChar(doc.src[end - 1])) {
            end -= 1;
        }

        if (end >= offset) return null;

        const receiver_name = doc.src[end..offset];

        log.debug("member completion: receiver='{s}'", .{receiver_name});
        log.debug("member completion: symbols in scope={d}", .{doc.getSymbolsInScope().len});

        if (Scope.BUILTIN_METHODS.get(receiver_name)) |methods| {
            var items = try arena.alloc(types.CompletionItem, methods.len);
            for (methods, 0..) |method, i| {
                items[i] = .{
                    .label = method.name,
                    .kind = .Method,
                    .detail = method.signature,
                };
            }
            log.debug("member completion: builtin methods={d}", .{methods.len});
            return items;
        }

        if (try self.getClassStaticCompletions(arena, doc, receiver_name, uri)) |items| {
            log.debug("member completion: class static methods={d}", .{items.len});
            return items;
        }

        for (doc.symbols.items) |sym| {
            if (std.mem.eql(u8, sym.name, receiver_name)) {
                log.debug("member completion: symbol match '{s}' kind={s}", .{ sym.name, @tagName(sym.kind) });
                if (sym.inferred_type) |inferred_type| {
                    const type_name = @tagName(inferred_type);
                    if (inferred_type == .class_type) {
                        if (sym.class_name) |class_name| {
                            if (try self.getClassInstanceCompletions(arena, doc, class_name, uri)) |items| {
                                log.debug("member completion: class instance methods={d}", .{items.len});
                                return items;
                            }
                        }
                        break;
                    }
                    if (Scope.INSTANCE_METHODS.get(type_name)) |methods| {
                        var items = try arena.alloc(types.CompletionItem, methods.len);
                        for (methods, 0..) |method, i| {
                            items[i] = .{
                                .label = method.name,
                                .kind = .Method,
                                .detail = method.signature,
                            };
                        }
                        log.debug("member completion: inferred type={s} methods={d}", .{ type_name, methods.len });
                        return items;
                    }
                    log.debug("member completion: no methods for inferred type={s}", .{type_name});
                } else {
                    log.debug("member completion: no inferred type for symbol '{s}'", .{sym.name});
                }
                break;
            }
        }

        log.debug("member completion: no symbol match for '{s}'", .{receiver_name});

        return null;
    }

    fn getImportPathCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        position: types.Position,
        uri: []const u8,
    ) !?[]types.CompletionItem {
        const offset = doc.positionToOffset(position.line, position.character) orelse return null;
        if (offset > doc.src.len) return null;

        const lines = doc.source_file.lines;
        const line_index: usize = @intCast(position.line);
        if (line_index >= lines.len) return null;

        const line_start = lines[line_index];
        const line_end = if (line_index + 1 < lines.len) lines[line_index + 1] else doc.src.len;
        if (offset < line_start or offset > line_end) return null;

        const line_slice = doc.src[line_start..line_end];
        const local_offset = offset - line_start;

        const import_index = std.mem.indexOf(u8, line_slice, "import") orelse return null;
        var i = import_index + "import".len;
        while (i < line_slice.len and std.ascii.isWhitespace(line_slice[i])) : (i += 1) {}
        if (i >= line_slice.len) return null;

        const quote = line_slice[i];
        if (quote != '"' and quote != '\'') return null;
        const quote_start = i + 1;
        const close_index = std.mem.indexOfScalarPos(u8, line_slice, quote_start, quote);

        if (local_offset < quote_start) return null;
        if (close_index) |close_pos| {
            if (local_offset > close_pos) return null;
        }

        const prefix = line_slice[quote_start..local_offset];
        if (self.getResolverImportCompletions(arena, uri, prefix)) |items| {
            return items;
        }
        return try listImportCompletions(arena, uri, prefix);
    }

    fn getResolverImportCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        prefix: []const u8,
    ) ?[]types.CompletionItem {
        const config = self.getConfig(uri);
        const project_root = config.project_root orelse ".";

        const chain = self.getResolverChain(uri) catch return null;
        const request = ResolveRequest{
            .importer_uri = uri,
            .importer_module_id = uri,
            .import_string = prefix,
            .project_root = project_root,
            .module_bases = config.modules,
        };

        if (chain.resolve(request)) |result| {
            if (result.completions.len == 0) return null;

            var items = arena.alloc(types.CompletionItem, result.completions.len) catch return null;
            for (result.completions, 0..) |completion, idx| {
                items[idx] = .{
                    .label = completion,
                    .kind = .File,
                    .insertText = completion,
                };
            }
            return items;
        }

        return null;
    }

    fn listImportCompletions(
        arena: std.mem.Allocator,
        uri: []const u8,
        prefix: []const u8,
    ) !?[]types.CompletionItem {
        const base_path = uriToPath(uri);
        const base_dir = std.fs.path.dirname(base_path) orelse base_path;

        var dir = try std.fs.cwd().openDir(base_dir, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(arena);
        defer walker.deinit();

        const trimmed_prefix = if (std.mem.startsWith(u8, prefix, "./")) prefix[2..] else prefix;
        const needs_dot = std.mem.startsWith(u8, prefix, "./");

        var items: std.ArrayListUnmanaged(types.CompletionItem) = .empty;

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.path, ".wren")) continue;

            const path_no_ext = entry.path[0 .. entry.path.len - 5];
            if (trimmed_prefix.len > 0 and !std.mem.startsWith(u8, path_no_ext, trimmed_prefix)) continue;

            const insert_text = if (needs_dot)
                try std.fmt.allocPrint(arena, "./{s}", .{path_no_ext})
            else
                try arena.dupe(u8, path_no_ext);

            try items.append(arena, .{
                .label = insert_text,
                .kind = .File,
                .insertText = insert_text,
            });
        }

        if (items.items.len == 0) return null;
        return items.items;
    }

    fn getClassStaticCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        receiver_name: []const u8,
        uri: []const u8,
    ) !?[]types.CompletionItem {
        if (try self.getClassStaticCompletionsInModule(arena, doc.module, receiver_name)) |items| {
            log.debug("member completion: class '{s}' found in current module", .{receiver_name});
            return items;
        }

        if (self.findImportPath(doc, receiver_name)) |import_path| {
            log.debug("member completion: import path for '{s}' is '{s}'", .{ receiver_name, import_path });
            const import_uri = try self.resolveImportUri(arena, uri, import_path);
            log.debug("member completion: resolved import uri '{s}'", .{import_uri});
            if (self.files.get(import_uri) == null) {
                self.loadImportedFile(import_uri) catch |err| {
                    log.debug("member completion: failed loading import '{s}' ({s})", .{ import_uri, @errorName(err) });
                };
            }

            const import_doc = self.files.get(import_uri) orelse return null;
            log.debug("member completion: imported doc found for '{s}'", .{import_uri});
            return self.getClassStaticCompletionsInModule(arena, import_doc.module, receiver_name);
        }

        if (try self.findImportModuleByClassName(arena, doc, receiver_name, uri)) |module| {
            return self.getClassStaticCompletionsInModule(arena, module, receiver_name);
        }

        return null;
    }

    fn getClassInstanceCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        class_name: []const u8,
        uri: []const u8,
    ) !?[]types.CompletionItem {
        if (try self.getClassInstanceCompletionsInModule(arena, doc.module, class_name)) |items| {
            return items;
        }

        const import_path = self.findImportPath(doc, class_name) orelse return null;
        const import_uri = try self.resolveImportUri(arena, uri, import_path);
        if (self.files.get(import_uri) == null) {
            self.loadImportedFile(import_uri) catch |err| {
                log.debug("member completion: failed loading import '{s}' ({s})", .{ import_uri, @errorName(err) });
            };
        }

        const import_doc = self.files.get(import_uri) orelse return null;
        return self.getClassInstanceCompletionsInModule(arena, import_doc.module, class_name);
    }

    fn getClassStaticCompletionsInModule(
        self: *Handler,
        arena: std.mem.Allocator,
        module: wrenalyzer.Ast.Module,
        receiver_name: []const u8,
    ) !?[]types.CompletionItem {
        _ = self;

        var class_node: ?wrenalyzer.Ast.ClassStmt = null;
        for (module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), receiver_name)) {
                            class_node = class_stmt;
                            break;
                        }
                    }
                },
                else => {},
            }
        }

        const class_stmt = class_node orelse return null;

        var count: usize = 0;
        for (class_stmt.methods) |method_node| {
            switch (method_node) {
                .Method => |method| {
                    const name_token = method.name orelse continue;
                    if (method.staticKeyword == null and method.constructKeyword == null) continue;
                    _ = name_token;
                    count += 1;
                },
                else => {},
            }
        }

        if (count == 0) return null;

        var items = try arena.alloc(types.CompletionItem, count);
        var idx: usize = 0;
        for (class_stmt.methods) |method_node| {
            switch (method_node) {
                .Method => |method| {
                    if (method.staticKeyword == null and method.constructKeyword == null) continue;
                    const name_token = method.name orelse continue;
                    items[idx] = .{
                        .label = name_token.name(),
                        .kind = .Method,
                        .detail = if (method.constructKeyword != null) "construct" else "static",
                    };
                    idx += 1;
                },
                else => {},
            }
        }

        return items[0..idx];
    }

    fn getClassInstanceCompletionsInModule(
        self: *Handler,
        arena: std.mem.Allocator,
        module: wrenalyzer.Ast.Module,
        class_name: []const u8,
    ) !?[]types.CompletionItem {
        _ = self;

        var class_node: ?wrenalyzer.Ast.ClassStmt = null;
        for (module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), class_name)) {
                            class_node = class_stmt;
                            break;
                        }
                    }
                },
                else => {},
            }
        }

        const class_stmt = class_node orelse return null;

        var count: usize = 0;
        for (class_stmt.methods) |method_node| {
            switch (method_node) {
                .Method => |method| {
                    if (method.staticKeyword != null) continue;
                    if (method.constructKeyword != null) continue;
                    if (method.name == null) continue;
                    count += 1;
                },
                else => {},
            }
        }

        if (count == 0) return null;

        var items = try arena.alloc(types.CompletionItem, count);
        var idx: usize = 0;
        for (class_stmt.methods) |method_node| {
            switch (method_node) {
                .Method => |method| {
                    if (method.staticKeyword != null) continue;
                    if (method.constructKeyword != null) continue;
                    const name_token = method.name orelse continue;
                    items[idx] = .{
                        .label = name_token.name(),
                        .kind = .Method,
                    };
                    idx += 1;
                },
                else => {},
            }
        }

        return items[0..idx];
    }

    fn findImportPath(self: *Handler, doc: Document, receiver_name: []const u8) ?[]const u8 {
        _ = self;

        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const variables = import_stmt.variables orelse continue;
                    const path_token = import_stmt.path orelse continue;
                    log.debug("member completion: import path token '{s}'", .{path_token.name()});
                    for (variables) |maybe_var| {
                        const var_token = maybe_var orelse continue;
                        if (std.mem.eql(u8, var_token.name(), receiver_name)) {
                            return stripQuotes(path_token.name());
                        }
                    }
                },
                else => {},
            }
        }

        return null;
    }

    fn findImportModuleByClassName(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        receiver_name: []const u8,
        uri: []const u8,
    ) !?wrenalyzer.Ast.Module {
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const path_token = import_stmt.path orelse continue;
                    const raw_path = stripQuotes(path_token.name());
                    if (!isImportFilePath(raw_path)) continue;

                    const import_uri = try self.resolveImportUri(arena, uri, raw_path);
                    if (self.files.get(import_uri) == null) {
                        self.loadImportedFile(import_uri) catch |err| {
                            log.debug("member completion: failed loading import '{s}' ({s})", .{ import_uri, @errorName(err) });
                        };
                    }

                    const import_doc = self.files.get(import_uri) orelse continue;
                    if (self.classExistsInModule(import_doc.module, receiver_name)) {
                        return import_doc.module;
                    }
                },
                else => {},
            }
        }

        return null;
    }

    fn classExistsInModule(self: *Handler, module: wrenalyzer.Ast.Module, class_name: []const u8) bool {
        _ = self;
        for (module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), class_name)) return true;
                    }
                },
                else => {},
            }
        }
        return false;
    }

    fn resolveImportUri(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        import_path: []const u8,
    ) ![]const u8 {
        const config = self.getConfig(uri);
        const project_root = config.project_root orelse ".";

        const chain = self.getResolverChain(uri) catch {
            return self.resolveImportUriFallback(arena, uri, import_path);
        };

        const request = ResolveRequest{
            .importer_uri = uri,
            .importer_module_id = uri,
            .import_string = import_path,
            .project_root = project_root,
            .module_bases = config.modules,
        };

        if (chain.resolve(request)) |result| {
            if (result.uri) |resolved_uri| {
                return arena.dupe(u8, resolved_uri);
            }
            return arena.dupe(u8, result.canonical_id);
        }

        return self.resolveImportUriFallback(arena, uri, import_path);
    }

    fn resolveImportUriFallback(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        import_path: []const u8,
    ) ![]const u8 {
        _ = self;

        const base_path = uriToPath(uri);
        const base_dir = std.fs.path.dirname(base_path) orelse base_path;
        const with_ext = try ensureWrenExtension(arena, import_path);
        const joined = try std.fs.path.join(arena, &.{ base_dir, with_ext });
        return std.fmt.allocPrint(arena, "file://{s}", .{joined});
    }

    fn isIdentChar(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    fn stripQuotes(value: []const u8) []const u8 {
        if (value.len >= 2 and value[0] == '"' and value[value.len - 1] == '"') {
            return value[1 .. value.len - 1];
        }
        return value;
    }

    fn uriToPath(uri: []const u8) []const u8 {
        const prefix = "file://";
        if (std.mem.startsWith(u8, uri, prefix)) {
            var path = uri[prefix.len..];
            // On Windows, file URIs are file:///C:/path, which becomes /C:/path
            // We need to remove the leading / on Windows
            if (builtin.os.tag == .windows and path.len > 2 and path[0] == '/' and path[2] == ':') {
                return path[1..];
            }
            return path;
        }
        return uri;
    }

    fn ensureWrenExtension(arena: std.mem.Allocator, path: []const u8) ![]const u8 {
        if (std.mem.endsWith(u8, path, ".wren")) return path;
        return std.fmt.allocPrint(arena, "{s}.wren", .{path});
    }

    fn checkImportPaths(self: *Handler, doc: *Document, uri: []const u8) !void {
        const config = self.getConfig(uri);
        const diag_config = config.diagnostics;
        const project_root = config.project_root orelse ".";

        const chain = self.getResolverChain(uri) catch null;

        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const path_token = import_stmt.path orelse continue;
                    const raw_path = stripQuotes(path_token.name());

                    if (std.mem.endsWith(u8, raw_path, ".wren")) {
                        if (diag_config.extension_in_import != .none) {
                            self.reportImportDiagnostic(
                                doc,
                                path_token,
                                "Import path should not include '.wren' extension",
                                diag_config.extension_in_import,
                            );
                        }
                        continue;
                    }

                    // Skip scheme-prefixed imports (e.g., "http:", "hl:")
                    if (hasSchemePrefix(raw_path)) {
                        continue;
                    }

                    var resolved_uri: ?[]const u8 = null;
                    var import_found = false;

                    if (chain) |c| {
                        const request = ResolveRequest{
                            .importer_uri = uri,
                            .importer_module_id = uri,
                            .import_string = raw_path,
                            .project_root = project_root,
                            .module_bases = config.modules,
                        };
                        if (c.resolve(request)) |result| {
                            self.reportResolverDiagnostics(doc, path_token, result.diagnostics);
                            if (result.kind == .file and result.uri != null) {
                                const resolved_path = uriToPath(result.uri.?);
                                if (std.fs.cwd().access(resolved_path, .{ .mode = .read_only })) |_| {
                                    import_found = true;
                                    resolved_uri = result.uri;
                                } else |_| {}
                            } else if (result.kind == .virtual) {
                                import_found = true;
                            }
                        }
                    } else {
                        if (!isImportFilePath(raw_path)) continue;

                        const base_path = uriToPath(uri);
                        const base_dir = std.fs.path.dirname(base_path) orelse base_path;
                        const with_ext = try ensureWrenExtension(self.gpa, raw_path);
                        defer if (with_ext.ptr != raw_path.ptr) self.gpa.free(with_ext);

                        const full_path = if (std.fs.path.isAbsolute(with_ext))
                            with_ext
                        else
                            try std.fs.path.join(self.gpa, &.{ base_dir, with_ext });
                        defer if (full_path.ptr != with_ext.ptr) self.gpa.free(full_path);

                        if (std.fs.cwd().access(full_path, .{ .mode = .read_only })) |_| {
                            import_found = true;
                            resolved_uri = std.fmt.allocPrint(self.gpa, "file://{s}", .{full_path}) catch null;
                        } else |_| {}
                    }

                    if (!import_found) {
                        if (diag_config.missing_import != .none) {
                            var buf: [256]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "Cannot find import '{s}'", .{raw_path}) catch "Cannot find import";
                            self.reportImportDiagnostic(doc, path_token, msg, diag_config.missing_import);
                        }
                        continue;
                    }

                    // Validate imported variables exist in the target module
                    if (diag_config.unknown_variable != .none) {
                        if (resolved_uri) |target_uri| {
                            self.checkImportedVariables(doc, import_stmt, target_uri, diag_config.unknown_variable);
                        }
                    }
                },
                else => {},
            }
        }
    }

    fn checkImportedVariables(
        self: *Handler,
        doc: *Document,
        import_stmt: wrenalyzer.Ast.ImportStmt,
        target_uri: []const u8,
        severity: DiagnosticSeverity,
    ) void {
        const variables = import_stmt.variables orelse return;

        // Try to load the target module if not already loaded
        if (self.files.get(target_uri) == null) {
            self.loadImportedFile(target_uri) catch return;
        }

        const target_doc = self.files.get(target_uri) orelse return;
        const target_module = target_doc.module;

        for (variables) |maybe_var| {
            const var_token = maybe_var orelse continue;
            const var_name = var_token.name();

            if (!self.symbolExistsInModule(target_module, var_name)) {
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "Module does not export '{s}'", .{var_name}) catch "Unknown export";
                self.reportImportDiagnostic(doc, var_token, msg, severity);
            }
        }
    }

    fn symbolExistsInModule(_: *Handler, module: wrenalyzer.Ast.Module, symbol_name: []const u8) bool {
        for (module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), symbol_name)) return true;
                    }
                },
                .VarStmt => |var_stmt| {
                    if (var_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), symbol_name)) return true;
                    }
                },
                else => {},
            }
        }
        return false;
    }

    fn reportImportDiagnostic(
        _: *Handler,
        doc: *Document,
        token: Token,
        message: []const u8,
        severity: DiagnosticSeverity,
    ) void {
        switch (severity) {
            .@"error" => doc.reporter.reportError(token, message),
            .warning => doc.reporter.reportWarning(token, message),
            .info, .hint => doc.reporter.reportInfo(token, message),
            .none => {},
        }
    }

    fn reportResolverDiagnostics(
        self: *Handler,
        doc: *Document,
        token: Token,
        diagnostics: []const ResolveResult.Diagnostic,
    ) void {
        for (diagnostics) |diag| {
            const severity = switch (diag.severity) {
                .@"error" => DiagnosticSeverity.@"error",
                .warning => DiagnosticSeverity.warning,
                .info => DiagnosticSeverity.info,
                .hint => DiagnosticSeverity.hint,
            };
            self.reportImportDiagnostic(doc, token, diag.message, severity);
        }
    }

    fn hasSchemePrefix(import_str: []const u8) bool {
        if (std.mem.indexOf(u8, import_str, ":")) |colon_pos| {
            if (colon_pos > 0 and colon_pos < import_str.len - 1) {
                const before_colon = import_str[0..colon_pos];
                for (before_colon) |c| {
                    if (!std.ascii.isAlphanumeric(c) and c != '-' and c != '_') {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    fn isImportFilePath(path: []const u8) bool {
        if (path.len == 0) return false;
        if (path[0] == '.') return true;
        return std.fs.path.isAbsolute(path);
    }

    fn loadImportedFile(self: *Handler, uri: []const u8) !void {
        const file_path = uriToPath(uri);
        const contents = try std.fs.cwd().readFileAlloc(self.gpa, file_path, 10 * 1024 * 1024);
        defer self.gpa.free(contents);

        const new_text = try self.gpa.alloc(u8, contents.len + 1);
        @memcpy(new_text[0..contents.len], contents);
        new_text[contents.len] = 0;

        const new_text_z: [:0]const u8 = new_text[0..contents.len :0];
        var doc = try Document.init(self.gpa, new_text_z, .wren);
        try self.checkImportPaths(&doc, uri);

        const gop = try self.files.getOrPut(self.gpa, uri);
        errdefer _ = self.files.remove(uri);

        if (gop.found_existing) {
            gop.value_ptr.deinit(self.gpa);
        } else {
            gop.key_ptr.* = try self.gpa.dupe(u8, uri);
        }

        gop.value_ptr.* = doc;
    }

    /// Load all imports for a file and build the import graph.
    fn loadImportsForFile(self: *Handler, arena: std.mem.Allocator, uri: []const u8) !void {
        const doc = self.files.get(uri) orelse return;
        log.info("loadImportsForFile: processing '{s}'", .{uri});

        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const path_token = import_stmt.path orelse continue;
                    const raw_path = stripQuotes(path_token.name());
                    log.info("loadImportsForFile: found import '{s}'", .{raw_path});

                    const import_uri = self.resolveImportUri(arena, uri, raw_path) catch |err| {
                        log.info("loadImportsForFile: resolveImportUri failed for '{s}': {s}", .{ raw_path, @errorName(err) });
                        continue;
                    };
                    log.info("loadImportsForFile: resolved '{s}' -> '{s}'", .{ raw_path, import_uri });

                    if (!std.mem.startsWith(u8, import_uri, "file://")) {
                        log.info("loadImportsForFile: skipping non-file URI '{s}'", .{import_uri});
                        continue;
                    }

                    if (self.files.get(import_uri) == null) {
                        self.loadImportedFile(import_uri) catch |err| {
                            log.info("loadImportsForFile: failed to load '{s}': {s}", .{ import_uri, @errorName(err) });
                            continue;
                        };
                        log.info("loadImportsForFile: loaded import file '{s}'", .{import_uri});
                    }

                    try self.registerImportEdge(uri, import_uri, import_stmt.variables);
                    log.info("loadImportsForFile: registered edge to '{s}'", .{import_uri});
                },
                else => {},
            }
        }
    }

    /// Register an import edge in the reverse import graph.
    fn registerImportEdge(
        self: *Handler,
        from_uri: []const u8,
        to_uri: []const u8,
        variables: ?[]?Token,
    ) !void {
        const gop = try self.reverse_imports.getOrPut(self.gpa, to_uri);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.gpa.dupe(u8, to_uri);
            gop.value_ptr.* = .empty;
        }

        var already_registered = false;
        for (gop.value_ptr.items) |existing| {
            if (std.mem.eql(u8, existing, from_uri)) {
                already_registered = true;
                break;
            }
        }
        if (!already_registered) {
            try gop.value_ptr.append(self.gpa, try self.gpa.dupe(u8, from_uri));
        }

        if (variables) |vars| {
            for (vars) |maybe_token| {
                if (maybe_token) |name_token| {
                    const symbol_name = name_token.name();
                    const key = try std.fmt.allocPrint(self.gpa, "{s}:{s}", .{ from_uri, symbol_name });

                    const sym_gop = try self.symbol_sources.getOrPut(self.gpa, key);
                    if (!sym_gop.found_existing) {
                        sym_gop.value_ptr.* = try self.gpa.dupe(u8, to_uri);
                    } else {
                        self.gpa.free(key);
                    }
                }
            }
        }
    }

    /// Get all files that import from the given module URI.
    fn getImporters(self: *Handler, module_uri: []const u8) []const []const u8 {
        if (self.reverse_imports.get(module_uri)) |importers| {
            return importers.items;
        }
        return &.{};
    }

    /// Find which module a symbol was imported from in a given file.
    fn getSymbolSourceModule(self: *Handler, arena: std.mem.Allocator, file_uri: []const u8, symbol_name: []const u8) ?[]const u8 {
        const key = std.fmt.allocPrint(arena, "{s}:{s}", .{ file_uri, symbol_name }) catch return null;
        return self.symbol_sources.get(key);
    }

    pub fn @"textDocument/hover"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.HoverParams,
    ) !?types.Hover {
        log.debug("hover: uri={s} line={d} col={d}", .{ params.textDocument.uri, params.position.line, params.position.character });

        const doc = self.files.get(params.textDocument.uri) orelse {
            log.debug("hover: document not found", .{});
            return null;
        };

        // First check if cursor is on a resolved reference (use site or declaration)
        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            const kind_str = switch (resolved.kind) {
                .variable => "variable",
                .parameter => "parameter",
                .class => "class",
                .method => "method",
                .field => "field",
                .static_field => "static field",
                .import_var => "import",
            };
            const name = resolved.use_token.name();
            const content = try std.fmt.allocPrint(arena, "**{s}** `{s}`", .{ kind_str, name });
            return .{
                .contents = .{ .MarkupContent = .{ .kind = .markdown, .value = content } },
                .range = tokenToRange(resolved.use_token),
            };
        }

        var range: ?types.Range = null;
        const sym = doc.findSymbolAtPosition(
            params.position.line,
            params.position.character,
        ) orelse blk: {
            const offset = doc.positionToOffset(params.position.line, params.position.character) orelse {
                log.debug("hover: no symbol at position", .{});
                return null;
            };

            if (offset >= doc.src.len) {
                log.debug("hover: offset out of range", .{});
                return null;
            }

            var start = offset;
            while (start > 0 and isIdentChar(doc.src[start - 1])) {
                start -= 1;
            }

            var end = offset;
            while (end < doc.src.len and isIdentChar(doc.src[end])) {
                end += 1;
            }

            if (start == end) {
                log.debug("hover: no ident at position", .{});
                return null;
            }

            const ident = doc.src[start..end];
            log.debug("hover: fallback ident '{s}'", .{ident});

            for (doc.getSymbolsInScope()) |fallback_sym| {
                if (std.mem.eql(u8, fallback_sym.name, ident)) {
                    range = rangeForOffsets(doc.source_file, start, end);
                    break :blk fallback_sym;
                }
            }

            log.debug("hover: no symbol match for ident", .{});
            return null;
        };

        log.debug("hover: symbol '{s}' kind={s}", .{ sym.name, @tagName(sym.kind) });

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
            .range = range orelse tokenToRange(sym.token),
        };
    }

    pub fn @"textDocument/definition"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.DefinitionParams,
    ) !lsp.ResultType("textDocument/definition") {
        log.info("definition: uri={s} line={d} col={d}", .{ params.textDocument.uri, params.position.line, params.position.character });

        const doc = self.files.get(params.textDocument.uri) orelse {
            log.info("definition: document not found", .{});
            return null;
        };

        // Use resolved reference index if available
        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            log.info("definition: resolved symbol '{s}' kind={s}", .{ resolved.use_token.name(), @tagName(resolved.kind) });

            // Check if this is an imported symbol - if so, jump to the source module
            if (resolved.kind == .import_var or resolved.kind == .class) {
                const symbol_name = resolved.use_token.name();
                log.info("definition: looking for source module of '{s}'", .{symbol_name});

                if (self.getSymbolSourceModule(arena, params.textDocument.uri, symbol_name)) |source_uri| {
                    log.info("definition: found source module '{s}'", .{source_uri});

                    // Load the source file if not already loaded
                    if (self.files.get(source_uri) == null) {
                        self.loadImportedFile(source_uri) catch {};
                    }

                    if (self.files.get(source_uri)) |source_doc| {
                        // Find the symbol definition in the source module
                        if (self.findSymbolInModule(source_doc, symbol_name)) |source_token| {
                            log.info("definition: found symbol in source module", .{});
                            return .{
                                .Definition = .{
                                    .Location = .{
                                        .uri = source_uri,
                                        .range = tokenToRange(source_token),
                                    },
                                },
                            };
                        } else {
                            log.info("definition: symbol not found in source module", .{});
                        }
                    } else {
                        log.info("definition: source doc not loaded", .{});
                    }
                } else {
                    log.info("definition: no source module found for '{s}'", .{symbol_name});
                }
            }

            if (resolved.kind == .method) {
                const method_name = resolved.use_token.name();
                log.info("definition: method call '{s}', looking for receiver class", .{method_name});

                if (self.findReceiverClassAtPosition(doc, resolved.use_token.start)) |receiver_name| {
                    log.info("definition: found receiver '{s}'", .{receiver_name});

                    var class_name: ?[]const u8 = null;
                    var source_uri: ?[]const u8 = null;
                    var method_kind: MethodKind = .instance;

                    if (self.getSymbolSourceModule(arena, params.textDocument.uri, receiver_name)) |uri| {
                        class_name = receiver_name;
                        source_uri = uri;
                        method_kind = .class_level;
                        log.info("definition: receiver is imported class '{s}'", .{receiver_name});
                    } else {
                        for (doc.symbols.items) |sym| {
                            if (std.mem.eql(u8, sym.name, receiver_name)) {
                                if (sym.kind == .class) {
                                    class_name = receiver_name;
                                    method_kind = .class_level;
                                    log.info("definition: receiver '{s}' is local class", .{receiver_name});
                                } else if (sym.class_name) |cn| {
                                    class_name = cn;
                                    method_kind = .instance;
                                    log.info("definition: receiver '{s}' has class_name '{s}'", .{ receiver_name, cn });
                                    source_uri = self.getSymbolSourceModule(arena, params.textDocument.uri, cn);
                                    if (source_uri) |uri| {
                                        log.info("definition: class '{s}' imported from '{s}'", .{ cn, uri });
                                    }
                                }
                                break;
                            }
                        }

                        if (class_name == null) {
                            if (self.findVariableClassFromSource(doc, receiver_name)) |cn| {
                                class_name = cn;
                                method_kind = .instance;
                                log.info("definition: inferred class '{s}' from source", .{cn});
                                source_uri = self.getSymbolSourceModule(arena, params.textDocument.uri, cn);
                            }
                        }
                    }

                    if (class_name != null and source_uri != null) {
                        if (self.files.get(source_uri.?) == null) {
                            self.loadImportedFile(source_uri.?) catch {};
                        }

                        if (self.files.get(source_uri.?)) |source_doc| {
                            if (findMethodInClassWithKind(source_doc, class_name.?, method_name, method_kind)) |method_token| {
                                log.info("definition: found method in source module", .{});
                                return .{
                                    .Definition = .{
                                        .Location = .{
                                            .uri = source_uri.?,
                                            .range = tokenToRange(method_token),
                                        },
                                    },
                                };
                            }
                            log.info("definition: method not found in class", .{});
                        }
                    } else if (class_name != null) {
                        if (findMethodInClassWithKind(doc, class_name.?, method_name, method_kind)) |method_token| {
                            log.info("definition: found method in local class", .{});
                            return .{
                                .Definition = .{
                                    .Location = .{
                                        .uri = params.textDocument.uri,
                                        .range = tokenToRange(method_token),
                                    },
                                },
                            };
                        }
                    }
                }
            }

            if (resolved.kind == .field or resolved.kind == .static_field) {
                const field_name = resolved.use_token.name();
                log.info("definition: field/property access '{s}'", .{field_name});

                if (self.findReceiverClassAtPosition(doc, resolved.use_token.start)) |receiver_name| {
                    log.info("definition: found receiver '{s}'", .{receiver_name});

                    var class_name: ?[]const u8 = null;
                    var source_uri: ?[]const u8 = null;

                    if (self.getSymbolSourceModule(arena, params.textDocument.uri, receiver_name)) |uri| {
                        class_name = receiver_name;
                        source_uri = uri;
                        log.info("definition: receiver is imported class '{s}'", .{receiver_name});
                    } else {
                        for (doc.symbols.items) |sym| {
                            if (std.mem.eql(u8, sym.name, receiver_name)) {
                                if (sym.kind == .class) {
                                    class_name = receiver_name;
                                    log.info("definition: receiver '{s}' is local class", .{receiver_name});
                                } else if (sym.class_name) |cn| {
                                    class_name = cn;
                                    log.info("definition: receiver '{s}' has class_name '{s}'", .{ receiver_name, cn });
                                    source_uri = self.getSymbolSourceModule(arena, params.textDocument.uri, cn);
                                    if (source_uri) |uri| {
                                        log.info("definition: class '{s}' imported from '{s}'", .{ cn, uri });
                                    }
                                }
                                break;
                            }
                        }

                        if (class_name == null) {
                            if (self.findVariableClassFromSource(doc, receiver_name)) |cn| {
                                class_name = cn;
                                log.info("definition: inferred class '{s}' from source", .{cn});
                                source_uri = self.getSymbolSourceModule(arena, params.textDocument.uri, cn);
                            }
                        }
                    }

                    if (class_name != null and source_uri != null) {
                        if (self.files.get(source_uri.?) == null) {
                            self.loadImportedFile(source_uri.?) catch {};
                        }

                        if (self.files.get(source_uri.?)) |source_doc| {
                            // Try field first
                            if (self.findFieldInClass(source_doc, class_name.?, field_name)) |field_token| {
                                log.info("definition: found field in source module", .{});
                                return .{
                                    .Definition = .{
                                        .Location = .{
                                            .uri = source_uri.?,
                                            .range = tokenToRange(field_token),
                                        },
                                    },
                                };
                            }
                            // Try getter method (property)
                            if (findMethodInClassWithKind(source_doc, class_name.?, field_name, .instance)) |method_token| {
                                log.info("definition: found property getter in source module", .{});
                                return .{
                                    .Definition = .{
                                        .Location = .{
                                            .uri = source_uri.?,
                                            .range = tokenToRange(method_token),
                                        },
                                    },
                                };
                            }
                            log.info("definition: property not found in class", .{});
                        }
                    } else if (class_name != null) {
                        // Try field first
                        if (self.findFieldInClass(doc, class_name.?, field_name)) |field_token| {
                            log.info("definition: found field in local class", .{});
                            return .{
                                .Definition = .{
                                    .Location = .{
                                        .uri = params.textDocument.uri,
                                        .range = tokenToRange(field_token),
                                    },
                                },
                            };
                        }
                        // Try getter method (property)
                        if (findMethodInClassWithKind(doc, class_name.?, field_name, .instance)) |method_token| {
                            log.info("definition: found property getter in local class", .{});
                            return .{
                                .Definition = .{
                                    .Location = .{
                                        .uri = params.textDocument.uri,
                                        .range = tokenToRange(method_token),
                                    },
                                },
                            };
                        }
                    }
                }
            }

            // Fall back to local definition
            log.info("definition: falling back to local definition", .{});
            return .{
                .Definition = .{
                    .Location = .{
                        .uri = params.textDocument.uri,
                        .range = tokenToRange(resolved.decl_token),
                    },
                },
            };
        }

        // Fallback: check if cursor is on a declaration itself
        log.info("definition: no resolved ref, checking declaration", .{});

        // Try to find property/field access in string interpolation or other context
        const offset = doc.positionToOffset(params.position.line, params.position.character) orelse {
            log.info("definition: could not convert position to offset", .{});
            return null;
        };

        log.info("definition: checking for property access at offset {d}", .{offset});

        // Look for pattern like "user.name" where cursor is on "name"
        if (self.findPropertyAccessAtOffset(arena, doc, offset)) |prop_access| {
            log.info("definition: FOUND PROPERTY ACCESS: receiver='{s}', field='{s}'", .{ prop_access.receiver, prop_access.field });

            if (self.resolvePropertyDefinition(arena, doc, prop_access.receiver, prop_access.field, params.textDocument.uri)) |result| {
                return result;
            }

            log.info("definition: property not found", .{});
        }

        const sym = doc.findSymbolAtPosition(
            params.position.line,
            params.position.character,
        ) orelse {
            log.info("definition: no symbol at position", .{});
            return null;
        };

        log.info("definition: found declaration '{s}' kind={s}", .{ sym.name, @tagName(sym.kind) });

        // If it's an import_var declaration, jump to the source module
        if (sym.kind == .import_var) {
            log.info("definition: import_var, looking for source module", .{});
            if (self.getSymbolSourceModule(arena, params.textDocument.uri, sym.name)) |source_uri| {
                log.info("definition: found source module '{s}'", .{source_uri});

                if (self.files.get(source_uri) == null) {
                    self.loadImportedFile(source_uri) catch {};
                }

                if (self.files.get(source_uri)) |source_doc| {
                    if (self.findSymbolInModule(source_doc, sym.name)) |source_token| {
                        log.info("definition: found symbol in source module", .{});
                        return .{
                            .Definition = .{
                                .Location = .{
                                    .uri = source_uri,
                                    .range = tokenToRange(source_token),
                                },
                            },
                        };
                    }
                }
            } else {
                log.info("definition: no source module for import_var '{s}'", .{sym.name});
            }
        }

        return .{
            .Definition = .{
                .Location = .{
                    .uri = params.textDocument.uri,
                    .range = tokenToRange(sym.token),
                },
            },
        };
    }

    /// Find a symbol (class, method) definition in a module.
    fn findSymbolInModule(_: *Handler, doc: Document, symbol_name: []const u8) ?Token {
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), symbol_name)) {
                            return name_token;
                        }
                    }
                },
                .VarStmt => |var_stmt| {
                    if (var_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), symbol_name)) {
                            return name_token;
                        }
                    }
                },
                else => {},
            }
        }

        for (doc.symbols.items) |sym| {
            if (std.mem.eql(u8, sym.name, symbol_name)) {
                return sym.token;
            }
        }

        return null;
    }

    /// Find the receiver identifier for a method call at the given token position.
    /// Uses AST traversal to find the CallExpr and extract its receiver.
    /// Returns null for chained calls, field receivers, or complex expressions.
    fn findReceiverClassAtPosition(_: *Handler, doc: Document, method_start: usize) ?[]const u8 {
        // Search for CallExpr with matching method name position
        for (doc.module.statements) |stmt| {
            if (findReceiverInNode(stmt, method_start)) |receiver_name| {
                return receiver_name;
            }
        }
        return null;
    }

    fn findReceiverInNode(node: wrenalyzer.Ast.Node, method_start: usize) ?[]const u8 {
        switch (node) {
            .CallExpr => |call| {
                // Check if this is the call we're looking for
                if (call.name.start == method_start) {
                    return extractSimpleReceiverName(call);
                }
                // Recurse into receiver
                if (call.receiver) |recv_ptr| {
                    if (findReceiverInNode(recv_ptr.*, method_start)) |name| {
                        return name;
                    }
                }
                // Recurse into arguments
                for (call.arguments) |arg| {
                    if (findReceiverInNode(arg, method_start)) |name| {
                        return name;
                    }
                }
            },
            .VarStmt => |var_stmt| {
                if (var_stmt.initializer.*) |initializer| {
                    if (findReceiverInNode(initializer, method_start)) |name| {
                        return name;
                    }
                }
            },
            .ClassStmt => |class_stmt| {
                for (class_stmt.methods) |method_node| {
                    if (findReceiverInNode(method_node, method_start)) |name| {
                        return name;
                    }
                }
            },
            .Method => |method| {
                if (method.body.*) |body| {
                    if (findReceiverInNode(body, method_start)) |name| {
                        return name;
                    }
                }
            },
            .Body => |body| {
                if (body.expression) |expr_ptr| {
                    if (findReceiverInNode(expr_ptr.*, method_start)) |name| {
                        return name;
                    }
                }
                if (body.statements) |stmts| {
                    for (stmts) |stmt| {
                        if (findReceiverInNode(stmt, method_start)) |name| {
                            return name;
                        }
                    }
                }
            },
            .BlockStmt => |block| {
                for (block.statements) |stmt| {
                    if (findReceiverInNode(stmt, method_start)) |name| {
                        return name;
                    }
                }
            },
            .IfStmt => |if_stmt| {
                if (findReceiverInNode(if_stmt.condition.*, method_start)) |name| {
                    return name;
                }
                if (findReceiverInNode(if_stmt.thenBranch.*, method_start)) |name| {
                    return name;
                }
                if (if_stmt.elseBranch) |else_ptr| {
                    if (findReceiverInNode(else_ptr.*, method_start)) |name| {
                        return name;
                    }
                }
            },
            .WhileStmt => |while_stmt| {
                if (findReceiverInNode(while_stmt.condition.*, method_start)) |name| {
                    return name;
                }
                if (findReceiverInNode(while_stmt.body.*, method_start)) |name| {
                    return name;
                }
            },
            .ForStmt => |for_stmt| {
                if (findReceiverInNode(for_stmt.iterator.*, method_start)) |name| {
                    return name;
                }
                if (findReceiverInNode(for_stmt.body.*, method_start)) |name| {
                    return name;
                }
            },
            .ReturnStmt => |ret| {
                if (ret.value.*) |val| {
                    if (findReceiverInNode(val, method_start)) |name| {
                        return name;
                    }
                }
            },
            .InfixExpr => |infix| {
                if (findReceiverInNode(infix.left.*, method_start)) |name| {
                    return name;
                }
                if (findReceiverInNode(infix.right.*, method_start)) |name| {
                    return name;
                }
            },
            .PrefixExpr => |prefix| {
                if (findReceiverInNode(prefix.right.*, method_start)) |name| {
                    return name;
                }
            },
            .AssignmentExpr => |assign| {
                if (findReceiverInNode(assign.value.*, method_start)) |name| {
                    return name;
                }
            },
            .GroupingExpr => |group| {
                if (findReceiverInNode(group.expression.*, method_start)) |name| {
                    return name;
                }
            },
            .ListExpr => |list| {
                for (list.elements) |elem| {
                    if (findReceiverInNode(elem, method_start)) |name| {
                        return name;
                    }
                }
            },
            .SubscriptExpr => |sub| {
                if (findReceiverInNode(sub.receiver.*, method_start)) |name| {
                    return name;
                }
                for (sub.arguments) |arg| {
                    if (findReceiverInNode(arg, method_start)) |name| {
                        return name;
                    }
                }
            },

            else => {},
        }
        return null;
    }

    /// Extract a simple identifier from a CallExpr's receiver.
    /// Returns null for chained calls (receiver has a receiver), field accesses,
    /// or complex expressions - only returns bare identifiers.
    fn extractSimpleReceiverName(call: wrenalyzer.Ast.CallExpr) ?[]const u8 {
        const receiver_ptr = call.receiver orelse return null;
        const receiver = receiver_ptr.*;

        switch (receiver) {
            // Bare identifier: `Foo.method()` - receiver is CallExpr with null receiver
            .CallExpr => |recv_call| {
                // Chained call: `foo.bar.method()` - receiver has its own receiver
                if (recv_call.receiver != null) return null;
                return recv_call.name.name();
            },
            // Field access: `_field.method()` - can't resolve type
            .FieldExpr => return null,
            .StaticFieldExpr => return null,
            // `this.method()` - return "this" for potential special handling
            .ThisExpr => return "this",
            // `super.method()` - return "super" for potential special handling
            .SuperExpr => return "super",
            // Complex expressions - can't resolve
            else => return null,
        }
    }

    const MethodKind = enum {
        instance, // callable on instance: !static && !construct
        class_level, // callable on class: static || construct
        any,
    };

    fn findMethodInClass(_: *Handler, doc: Document, class_name: []const u8, method_name: []const u8) ?Token {
        return findMethodInClassWithKind(doc, class_name, method_name, .any);
    }

    fn findMethodInClassWithKind(doc: Document, class_name: []const u8, method_name: []const u8, kind: MethodKind) ?Token {
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    const cls_name_token = class_stmt.name orelse continue;
                    if (!std.mem.eql(u8, cls_name_token.name(), class_name)) continue;

                    for (class_stmt.methods) |method_node| {
                        switch (method_node) {
                            .Method => |method| {
                                const name_token = method.name orelse continue;
                                const token_name = name_token.name();

                                const is_static = method.staticKeyword != null;
                                const is_constructor = method.constructKeyword != null;

                                const kind_matches = switch (kind) {
                                    .any => true,
                                    .class_level => is_static or is_constructor,
                                    .instance => !is_static and !is_constructor,
                                };
                                if (!kind_matches) continue;

                                if (std.mem.eql(u8, token_name, method_name)) {
                                    return name_token;
                                }

                                if (std.mem.startsWith(u8, token_name, method_name)) {
                                    if (token_name.len > method_name.len) {
                                        const next_char = token_name[method_name.len];
                                        if (next_char == '(' or next_char == '=') {
                                            return name_token;
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        return null;
    }

    /// Property access result from parsing "receiver.field"
    const PropertyAccess = struct {
        receiver: []const u8,
        field: []const u8,
    };

    /// Try to find "receiver.field" property access using AST tokens.
    fn findPropertyAccessAtOffset(self: *Handler, arena: std.mem.Allocator, doc: Document, offset: usize) ?PropertyAccess {
        for (doc.module.statements) |stmt| {
            if (self.findPropertyAccessInNode(arena, stmt, offset)) |access| {
                return access;
            }
        }

        return null;
    }

    fn findPropertyAccessInNode(
        self: *Handler,
        arena: std.mem.Allocator,
        node: wrenalyzer.Ast.Node,
        offset: usize,
    ) ?PropertyAccess {
        switch (node) {
            .CallExpr => |call| {
                const start = call.name.start;
                const end = start + call.name.length;
                if (offset >= start and offset < end) {
                    if (call.receiver) |receiver_ptr| {
                        if (self.extractPropertyReceiverName(receiver_ptr.*)) |receiver_name| {
                            const receiver_dup = arena.dupe(u8, receiver_name) catch return null;
                            const field_dup = arena.dupe(u8, call.name.name()) catch return null;
                            log.info("definition: parsed property access: '{s}.{s}'", .{ receiver_dup, field_dup });
                            return .{ .receiver = receiver_dup, .field = field_dup };
                        }
                    }
                }

                if (call.receiver) |recv_ptr| {
                    if (self.findPropertyAccessInNode(arena, recv_ptr.*, offset)) |access| {
                        return access;
                    }
                }
                for (call.arguments) |arg| {
                    if (self.findPropertyAccessInNode(arena, arg, offset)) |access| {
                        return access;
                    }
                }
                if (call.blockArgument.*) |block| {
                    if (self.findPropertyAccessInNode(arena, block, offset)) |access| {
                        return access;
                    }
                }
            },
            .VarStmt => |var_stmt| {
                if (var_stmt.initializer.*) |initializer| {
                    if (self.findPropertyAccessInNode(arena, initializer, offset)) |access| {
                        return access;
                    }
                }
            },
            .ClassStmt => |class_stmt| {
                for (class_stmt.methods) |method_node| {
                    if (self.findPropertyAccessInNode(arena, method_node, offset)) |access| {
                        return access;
                    }
                }
            },
            .Method => |method| {
                if (method.body.*) |body| {
                    if (self.findPropertyAccessInNode(arena, body, offset)) |access| {
                        return access;
                    }
                }
            },
            .Body => |body| {
                if (body.expression) |expr_ptr| {
                    if (self.findPropertyAccessInNode(arena, expr_ptr.*, offset)) |access| {
                        return access;
                    }
                }
                if (body.statements) |stmts| {
                    for (stmts) |stmt| {
                        if (self.findPropertyAccessInNode(arena, stmt, offset)) |access| {
                            return access;
                        }
                    }
                }
            },
            .BlockStmt => |block| {
                for (block.statements) |stmt| {
                    if (self.findPropertyAccessInNode(arena, stmt, offset)) |access| {
                        return access;
                    }
                }
            },
            .IfStmt => |if_stmt| {
                if (self.findPropertyAccessInNode(arena, if_stmt.condition.*, offset)) |access| {
                    return access;
                }
                if (self.findPropertyAccessInNode(arena, if_stmt.thenBranch.*, offset)) |access| {
                    return access;
                }
                if (if_stmt.elseBranch) |else_ptr| {
                    if (self.findPropertyAccessInNode(arena, else_ptr.*, offset)) |access| {
                        return access;
                    }
                }
            },
            .WhileStmt => |while_stmt| {
                if (self.findPropertyAccessInNode(arena, while_stmt.condition.*, offset)) |access| {
                    return access;
                }
                if (self.findPropertyAccessInNode(arena, while_stmt.body.*, offset)) |access| {
                    return access;
                }
            },
            .ForStmt => |for_stmt| {
                if (self.findPropertyAccessInNode(arena, for_stmt.iterator.*, offset)) |access| {
                    return access;
                }
                if (self.findPropertyAccessInNode(arena, for_stmt.body.*, offset)) |access| {
                    return access;
                }
            },
            .ReturnStmt => |ret| {
                if (ret.value.*) |val| {
                    if (self.findPropertyAccessInNode(arena, val, offset)) |access| {
                        return access;
                    }
                }
            },
            .InfixExpr => |infix| {
                if (self.findPropertyAccessInNode(arena, infix.left.*, offset)) |access| {
                    return access;
                }
                if (self.findPropertyAccessInNode(arena, infix.right.*, offset)) |access| {
                    return access;
                }
            },
            .PrefixExpr => |prefix| {
                if (self.findPropertyAccessInNode(arena, prefix.right.*, offset)) |access| {
                    return access;
                }
            },
            .AssignmentExpr => |assign| {
                if (self.findPropertyAccessInNode(arena, assign.value.*, offset)) |access| {
                    return access;
                }
            },
            .GroupingExpr => |group| {
                if (self.findPropertyAccessInNode(arena, group.expression.*, offset)) |access| {
                    return access;
                }
            },
            .ListExpr => |list| {
                for (list.elements) |elem| {
                    if (self.findPropertyAccessInNode(arena, elem, offset)) |access| {
                        return access;
                    }
                }
            },
            .SubscriptExpr => |sub| {
                if (self.findPropertyAccessInNode(arena, sub.receiver.*, offset)) |access| {
                    return access;
                }
                for (sub.arguments) |arg| {
                    if (self.findPropertyAccessInNode(arena, arg, offset)) |access| {
                        return access;
                    }
                }
            },
            else => {},
        }

        return null;
    }

    fn extractPropertyReceiverName(_: *Handler, receiver: wrenalyzer.Ast.Node) ?[]const u8 {
        switch (receiver) {
            .CallExpr => |call| {
                if (call.receiver != null) return null;
                return call.name.name();
            },
            .ThisExpr => return "this",
            .SuperExpr => return "super",
            else => return null,
        }
    }

    /// Resolve a property (receiver.field) to its definition.
    /// Returns Definition location if found, null otherwise.
    fn resolvePropertyDefinition(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        receiver_name: []const u8,
        field_name: []const u8,
        current_uri: []const u8,
    ) ?lsp.ResultType("textDocument/definition") {
        var class_name: ?[]const u8 = null;
        var source_uri: ?[]const u8 = null;

        if (self.getSymbolSourceModule(arena, current_uri, receiver_name)) |uri| {
            class_name = receiver_name;
            source_uri = uri;
            log.info("definition: receiver is imported class '{s}'", .{receiver_name});
        } else {
            for (doc.symbols.items) |sym| {
                if (std.mem.eql(u8, sym.name, receiver_name)) {
                    if (sym.kind == .class) {
                        class_name = receiver_name;
                        log.info("definition: receiver is local class", .{});
                    } else if (sym.class_name) |cn| {
                        class_name = cn;
                        log.info("definition: receiver has class_name '{s}'", .{cn});
                        source_uri = self.getSymbolSourceModule(arena, current_uri, cn);
                    }
                    break;
                }
            }

            if (class_name == null) {
                if (self.findVariableClassFromSource(doc, receiver_name)) |cn| {
                    class_name = cn;
                    log.info("definition: inferred class '{s}' from source", .{cn});
                    source_uri = self.getSymbolSourceModule(arena, current_uri, cn);
                }
            }
        }

        if (class_name != null and source_uri != null) {
            if (self.files.get(source_uri.?) == null) {
                self.loadImportedFile(source_uri.?) catch {};
            }

            if (self.files.get(source_uri.?)) |source_doc| {
                // Try field first
                if (self.findFieldInClass(source_doc, class_name.?, field_name)) |field_token| {
                    log.info("definition: found field in source module", .{});
                    return .{
                        .Definition = .{
                            .Location = .{
                                .uri = source_uri.?,
                                .range = tokenToRange(field_token),
                            },
                        },
                    };
                }
                // Try getter method
                if (findMethodInClassWithKind(source_doc, class_name.?, field_name, .instance)) |method_token| {
                    log.info("definition: found property getter in source module", .{});
                    return .{
                        .Definition = .{
                            .Location = .{
                                .uri = source_uri.?,
                                .range = tokenToRange(method_token),
                            },
                        },
                    };
                }
            }
        } else if (class_name != null) {
            // Local class
            if (self.findFieldInClass(doc, class_name.?, field_name)) |field_token| {
                log.info("definition: found field in local class", .{});
                return .{
                    .Definition = .{
                        .Location = .{
                            .uri = current_uri,
                            .range = tokenToRange(field_token),
                        },
                    },
                };
            }
            if (findMethodInClassWithKind(doc, class_name.?, field_name, .instance)) |method_token| {
                log.info("definition: found property getter in local class", .{});
                return .{
                    .Definition = .{
                        .Location = .{
                            .uri = current_uri,
                            .range = tokenToRange(method_token),
                        },
                    },
                };
            }
        }

        return null;
    }

    fn findFieldInClass(_: *Handler, doc: Document, class_name: []const u8, field_name: []const u8) ?Token {
        // Fields in wrenalyzer are tracked as symbols with kind .field or .static_field
        // and have a class_name pointing to the owning class
        for (doc.symbols.items) |sym| {
            if ((sym.kind == .field or sym.kind == .static_field) and
                sym.class_name != null and
                std.mem.eql(u8, sym.class_name.?, class_name) and
                std.mem.eql(u8, sym.name, field_name))
            {
                return sym.token;
            }
        }

        return null;
    }

    /// Find the class name for a variable by examining its initializer in the AST.
    /// Looks for patterns like `var x = ClassName.new(...)` where the initializer
    /// is a constructor call.
    fn findVariableClassFromSource(_: *Handler, doc: Document, var_name: []const u8) ?[]const u8 {
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .VarStmt => |var_stmt| {
                    const name_token = var_stmt.name orelse continue;
                    if (!std.mem.eql(u8, name_token.name(), var_name)) continue;

                    const init_ptr = var_stmt.initializer;
                    const initializer = init_ptr.* orelse continue;

                    // Check if initializer is a CallExpr (method call)
                    switch (initializer) {
                        .CallExpr => |call| {
                            // Must be a call to "new" (constructor)
                            if (!std.mem.eql(u8, call.name.name(), "new")) continue;

                            // Receiver must exist and be an identifier (class name)
                            const receiver_ptr = call.receiver orelse continue;
                            const receiver = receiver_ptr.*;

                            // Receiver should be a CallExpr with null receiver (bare identifier)
                            switch (receiver) {
                                .CallExpr => |recv_call| {
                                    if (recv_call.receiver != null) continue;
                                    return recv_call.name.name();
                                },
                                else => continue,
                            }
                        },
                        else => continue,
                    }
                },
                .ClassStmt => |class_stmt| {
                    // Also search inside class methods for local variables
                    for (class_stmt.methods) |method_node| {
                        switch (method_node) {
                            .Method => |method| {
                                const body_ptr = method.body;
                                const body_node = body_ptr.* orelse continue;
                                if (findVariableClassInBody(body_node, var_name)) |class_name| {
                                    return class_name;
                                }
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }

        return null;
    }

    fn findVariableClassInBody(body_node: wrenalyzer.Ast.Node, var_name: []const u8) ?[]const u8 {
        switch (body_node) {
            .Body => |body| {
                const stmts = body.statements orelse return null;
                for (stmts) |stmt| {
                    switch (stmt) {
                        .VarStmt => |var_stmt| {
                            const name_token = var_stmt.name orelse continue;
                            if (!std.mem.eql(u8, name_token.name(), var_name)) continue;

                            const init_ptr = var_stmt.initializer;
                            const initializer = init_ptr.* orelse continue;

                            switch (initializer) {
                                .CallExpr => |call| {
                                    if (!std.mem.eql(u8, call.name.name(), "new")) continue;

                                    const receiver_ptr = call.receiver orelse continue;
                                    const receiver = receiver_ptr.*;

                                    switch (receiver) {
                                        .CallExpr => |recv_call| {
                                            if (recv_call.receiver != null) continue;
                                            return recv_call.name.name();
                                        },
                                        else => continue,
                                    }
                                },
                                else => continue,
                            }
                        },
                        .BlockStmt => |block| {
                            for (block.statements) |inner| {
                                if (findVariableClassInBody(inner, var_name)) |class_name| {
                                    return class_name;
                                }
                            }
                        },
                        .IfStmt => |if_stmt| {
                            if (findVariableClassInBody(if_stmt.thenBranch.*, var_name)) |class_name| {
                                return class_name;
                            }
                            if (if_stmt.elseBranch) |else_ptr| {
                                if (findVariableClassInBody(else_ptr.*, var_name)) |class_name| {
                                    return class_name;
                                }
                            }
                        },
                        .WhileStmt => |while_stmt| {
                            if (findVariableClassInBody(while_stmt.body.*, var_name)) |class_name| {
                                return class_name;
                            }
                        },
                        .ForStmt => |for_stmt| {
                            if (findVariableClassInBody(for_stmt.body.*, var_name)) |class_name| {
                                return class_name;
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
        return null;
    }

    pub fn @"textDocument/references"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.ReferenceParams,
    ) !lsp.ResultType("textDocument/references") {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        const include_decl = params.context.includeDeclaration;

        var decl_token_start: ?usize = null;
        var symbol_name: ?[]const u8 = null;
        var symbol_kind: ?Scope.Symbol.Kind = null;

        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            decl_token_start = resolved.decl_token.start;
            symbol_name = resolved.use_token.name();
            symbol_kind = resolved.kind;
        } else {
            if (doc.findSymbolAtPosition(params.position.line, params.position.character)) |sym| {
                decl_token_start = sym.token.start;
                symbol_name = sym.name;
                symbol_kind = sym.kind;
            }
        }

        const target_start = decl_token_start orelse return null;
        const target_name = symbol_name orelse return null;

        var locations: std.ArrayListUnmanaged(types.Location) = .empty;

        // Find all uses in current file that refer to this declaration
        for (doc.refs.items) |ref| {
            if (ref.decl_token.start == target_start) {
                try locations.append(arena, .{
                    .uri = params.textDocument.uri,
                    .range = tokenToRange(ref.use_token),
                });
            }
        }

        // Include declaration if requested
        if (include_decl) {
            for (doc.symbols.items) |sym| {
                if (sym.token.start == target_start) {
                    try locations.append(arena, .{
                        .uri = params.textDocument.uri,
                        .range = tokenToRange(sym.token),
                    });
                    break;
                }
            }
        }

        // Cross-file references: if this is a class/method, search files that import this module
        if (symbol_kind) |kind| {
            if (kind == .class or kind == .method) {
                const importers = self.getImporters(params.textDocument.uri);
                for (importers) |importer_uri| {
                    const importer_doc = self.files.get(importer_uri) orelse continue;

                    // Check if this importer actually imports the symbol we're looking for
                    if (!self.fileImportsSymbol(importer_doc, target_name, params.textDocument.uri)) {
                        continue;
                    }

                    // Find uses of this symbol in the importing file
                    for (importer_doc.refs.items) |ref| {
                        if (std.mem.eql(u8, ref.use_token.name(), target_name)) {
                            // Check that this is an import_var kind (imported symbol)
                            if (ref.kind == .import_var or ref.kind == .class) {
                                try locations.append(arena, .{
                                    .uri = importer_uri,
                                    .range = tokenToRange(ref.use_token),
                                });
                            }
                        }
                    }

                    // Also check symbols (for class instantiation patterns)
                    for (importer_doc.symbols.items) |sym| {
                        if (std.mem.eql(u8, sym.name, target_name) and sym.kind == .import_var) {
                            try locations.append(arena, .{
                                .uri = importer_uri,
                                .range = tokenToRange(sym.token),
                            });
                        }
                    }
                }
            }
        }

        if (locations.items.len == 0) return null;
        return locations.items;
    }

    /// Check if a file imports a specific symbol from a specific module.
    fn fileImportsSymbol(self: *Handler, doc: Document, symbol_name: []const u8, from_uri: []const u8) bool {
        _ = self;
        _ = from_uri;
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    if (import_stmt.variables) |vars| {
                        for (vars) |maybe_token| {
                            if (maybe_token) |name_token| {
                                if (std.mem.eql(u8, name_token.name(), symbol_name)) {
                                    return true;
                                }
                            }
                        }
                    }
                },
                else => {},
            }
        }
        return false;
    }

    pub fn @"textDocument/rename"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.RenameParams,
    ) !?types.WorkspaceEdit {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        // Use resolved reference index if available
        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            var edits: std.ArrayListUnmanaged(types.TextEdit) = .empty;

            // Track which declaration starts we've already added to avoid duplicates
            var added_decl = false;

            // Find all references to this declaration
            for (doc.refs.items) |ref| {
                if (ref.decl_token.start == resolved.decl_token.start) {
                    try edits.append(arena, .{
                        .range = tokenToRange(ref.use_token),
                        .newText = params.newName,
                    });
                }
            }

            // Also include the declaration itself (check doc.symbols first for module-level)
            for (doc.symbols.items) |sym| {
                if (sym.token.start == resolved.decl_token.start) {
                    try edits.append(arena, .{
                        .range = tokenToRange(sym.token),
                        .newText = params.newName,
                    });
                    added_decl = true;
                    break;
                }
            }

            // If declaration not found in symbols (e.g., local variable inside method),
            // add it directly from the resolved ref's decl_token
            if (!added_decl) {
                try edits.append(arena, .{
                    .range = tokenToRange(resolved.decl_token),
                    .newText = params.newName,
                });
            }

            if (edits.items.len == 0) return null;

            const uri = params.textDocument.uri;
            const uri_edits = try arena.alloc(types.TextEdit, edits.items.len);
            @memcpy(uri_edits, edits.items);

            var changes = lsp.parser.Map(types.DocumentUri, []const types.TextEdit){};
            try changes.map.put(arena, uri, uri_edits);

            return .{ .changes = changes };
        }

        // Fallback to old implementation
        const sym = doc.findSymbolAtPosition(
            params.position.line,
            params.position.character,
        ) orelse blk: {
            const offset = doc.positionToOffset(params.position.line, params.position.character) orelse return null;
            if (offset >= doc.src.len) return null;

            var start = offset;
            while (start > 0 and isIdentChar(doc.src[start - 1])) {
                start -= 1;
            }

            var end = offset;
            while (end < doc.src.len and isIdentChar(doc.src[end])) {
                end += 1;
            }

            if (start == end) return null;

            const ident = doc.src[start..end];
            for (doc.getSymbolsInScope()) |fallback_sym| {
                if (std.mem.eql(u8, fallback_sym.name, ident)) {
                    break :blk fallback_sym;
                }
            }

            return null;
        };

        var edits: std.ArrayListUnmanaged(types.TextEdit) = .empty;

        var lexer = try wrenalyzer.Lexer.new(arena, doc.source_file);
        while (true) {
            const token = try lexer.readToken();
            if (token.type == .eof) break;

            switch (token.type) {
                .name, .field, .staticField => {},
                else => continue,
            }

            if (!std.mem.eql(u8, token.name(), sym.name)) continue;

            try edits.append(arena, .{
                .range = tokenToRange(token),
                .newText = params.newName,
            });
        }

        if (edits.items.len == 0) return null;

        const uri = params.textDocument.uri;
        const uri_edits = try arena.alloc(types.TextEdit, edits.items.len);
        @memcpy(uri_edits, edits.items);

        var changes = lsp.parser.Map(types.DocumentUri, []const types.TextEdit){};
        try changes.map.put(arena, uri, uri_edits);

        return .{ .changes = changes };
    }

    pub fn @"textDocument/documentHighlight"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.DocumentHighlightParams,
    ) !?[]const types.DocumentHighlight {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        var decl_token_start: ?usize = null;

        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            decl_token_start = resolved.decl_token.start;
        } else {
            if (doc.findSymbolAtPosition(params.position.line, params.position.character)) |sym| {
                decl_token_start = sym.token.start;
            }
        }

        const target_start = decl_token_start orelse return null;

        var highlights: std.ArrayListUnmanaged(types.DocumentHighlight) = .empty;
        var added_decl = false;

        for (doc.refs.items) |ref| {
            if (ref.decl_token.start == target_start) {
                try highlights.append(arena, .{
                    .range = tokenToRange(ref.use_token),
                    .kind = if (ref.is_write) .Write else .Read,
                });
            }
        }

        // Check module-level symbols for the declaration
        for (doc.symbols.items) |sym| {
            if (sym.token.start == target_start) {
                try highlights.append(arena, .{
                    .range = tokenToRange(sym.token),
                    .kind = .Write,
                });
                added_decl = true;
                break;
            }
        }

        // If declaration not in symbols (local variable), get it from any ref
        if (!added_decl) {
            for (doc.refs.items) |ref| {
                if (ref.decl_token.start == target_start) {
                    try highlights.append(arena, .{
                        .range = tokenToRange(ref.decl_token),
                        .kind = .Write,
                    });
                    break;
                }
            }
        }

        if (highlights.items.len == 0) return null;
        return highlights.items;
    }

    pub fn @"textDocument/codeAction"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.CodeActionParams,
    ) !lsp.ResultType("textDocument/codeAction") {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        const code_action_slice = std.meta.Child(lsp.ResultType("textDocument/codeAction"));
        const CodeActionItem = std.meta.Child(code_action_slice);
        var actions: std.ArrayListUnmanaged(CodeActionItem) = .empty;

        // Find diagnostics in the requested range
        for (doc.getDiagnostics()) |diag| {
            const diag_range = tokenToRange(diag.token);

            // Check if diagnostic overlaps with requested range
            if (diag_range.start.line > params.range.end.line or
                diag_range.end.line < params.range.start.line)
            {
                continue;
            }

            const message = diag.message;
            const token_name = diag.token.name();

            // Create LSP diagnostic for association
            var diag_arr = try arena.alloc(types.Diagnostic, 1);
            diag_arr[0] = .{
                .range = diag_range,
                .severity = switch (diag.severity) {
                    .@"error" => .Error,
                    .warning => .Warning,
                    .info => .Information,
                    .hint => .Hint,
                },
                .message = try arena.dupe(u8, message),
            };

            // Undefined variable - suggest creating a variable declaration
            if (std.mem.containsAtLeast(u8, message, 1, "not defined")) {
                // Create edit to add variable declaration at start of current scope
                var changes = lsp.parser.Map(types.DocumentUri, []const types.TextEdit){};

                // Find a good insertion point (start of file for now)
                const insert_text = try std.fmt.allocPrint(arena, "var {s} = null\n", .{token_name});
                var edits = try arena.alloc(types.TextEdit, 1);
                edits[0] = .{
                    .range = .{
                        .start = .{ .line = 0, .character = 0 },
                        .end = .{ .line = 0, .character = 0 },
                    },
                    .newText = insert_text,
                };
                try changes.map.put(arena, params.textDocument.uri, edits);

                try actions.append(arena, .{ .CodeAction = .{
                    .title = try std.fmt.allocPrint(arena, "Create variable '{s}'", .{token_name}),
                    .kind = .quickfix,
                    .diagnostics = diag_arr,
                    .edit = .{ .changes = changes },
                } });
            }

            // Incorrect argument count - provide info action (no auto-fix, too complex)
            if (std.mem.containsAtLeast(u8, message, 1, "Expected") and
                std.mem.containsAtLeast(u8, message, 1, "arguments"))
            {
                try actions.append(arena, .{ .CodeAction = .{
                    .title = "Incorrect argument count",
                    .kind = .quickfix,
                    .diagnostics = diag_arr,
                    .isPreferred = false,
                } });
            }

            // Unknown method - suggest similar methods (placeholder for now)
            if (std.mem.containsAtLeast(u8, message, 1, "Unknown") and
                std.mem.containsAtLeast(u8, message, 1, "method"))
            {
                try actions.append(arena, .{ .CodeAction = .{
                    .title = "Check method name spelling",
                    .kind = .quickfix,
                    .diagnostics = diag_arr,
                } });
            }
        }

        if (actions.items.len == 0) return null;
        return actions.items;
    }

    pub fn @"workspace/symbol"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.WorkspaceSymbolParams,
    ) !lsp.ResultType("workspace/symbol") {
        const query = params.query;
        const query_lower = try std.ascii.allocLowerString(arena, query);
        var symbols: std.ArrayListUnmanaged(types.SymbolInformation) = .empty;

        var iter = self.files.iterator();
        while (iter.next()) |entry| {
            const uri = entry.key_ptr.*;
            const doc = entry.value_ptr.*;

            for (doc.module.statements) |stmt| {
                switch (stmt) {
                    .ClassStmt => |class_stmt| {
                        if (class_stmt.name) |name_token| {
                            const class_name = name_token.name();
                            const class_name_lower = try std.ascii.allocLowerString(arena, class_name);

                            if (query.len == 0 or std.mem.indexOf(u8, class_name_lower, query_lower) != null) {
                                try symbols.append(arena, .{
                                    .name = class_name,
                                    .kind = .Class,
                                    .deprecated = false,
                                    .location = .{
                                        .uri = uri,
                                        .range = tokenToRange(name_token),
                                    },
                                    .containerName = null,
                                });
                            }

                            for (class_stmt.methods) |method| {
                                switch (method) {
                                    .Method => |m| {
                                        if (m.name) |method_name_token| {
                                            const method_name = method_name_token.name();
                                            const method_name_lower = try std.ascii.allocLowerString(arena, method_name);

                                            if (query.len == 0 or std.mem.indexOf(u8, method_name_lower, query_lower) != null) {
                                                try symbols.append(arena, .{
                                                    .name = method_name,
                                                    .kind = .Method,
                                                    .deprecated = false,
                                                    .location = .{
                                                        .uri = uri,
                                                        .range = tokenToRange(method_name_token),
                                                    },
                                                    .containerName = class_name,
                                                });
                                            }
                                        }
                                    },
                                    else => {},
                                }
                            }
                        }
                    },
                    .VarStmt => |var_stmt| {
                        if (var_stmt.name) |name_token| {
                            const var_name = name_token.name();
                            const var_name_lower = try std.ascii.allocLowerString(arena, var_name);

                            if (query.len == 0 or std.mem.indexOf(u8, var_name_lower, query_lower) != null) {
                                try symbols.append(arena, .{
                                    .name = var_name,
                                    .kind = .Variable,
                                    .deprecated = false,
                                    .location = .{
                                        .uri = uri,
                                        .range = tokenToRange(name_token),
                                    },
                                    .containerName = null,
                                });
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        if (symbols.items.len == 0) return null;
        return .{ .array_of_SymbolInformation = symbols.items };
    }

    pub fn @"textDocument/foldingRange"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.FoldingRangeParams,
    ) !?[]const types.FoldingRange {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        var ranges: std.ArrayListUnmanaged(types.FoldingRange) = .empty;

        // Scan source for brace pairs
        var brace_stack: std.ArrayListUnmanaged(struct { offset: usize, line: u32 }) = .empty;
        defer brace_stack.deinit(arena);

        var lexer = try wrenalyzer.Lexer.new(arena, doc.source_file);
        while (true) {
            const token = try lexer.readToken();
            if (token.type == .eof) break;

            if (token.type == .leftBrace) {
                try brace_stack.append(arena, .{
                    .offset = token.start,
                    .line = @intCast(doc.source_file.lineAt(token.start) -| 1),
                });
            } else if (token.type == .rightBrace) {
                if (brace_stack.items.len > 0) {
                    const open = brace_stack.items[brace_stack.items.len - 1];
                    _ = brace_stack.pop();
                    const close_line: u32 = @intCast(doc.source_file.lineAt(token.start) -| 1);
                    if (close_line > open.line) {
                        try ranges.append(arena, .{
                            .startLine = open.line,
                            .endLine = close_line,
                            .kind = .region,
                        });
                    }
                }
            }
        }

        // Scan for multi-line block comments (/* */)
        const source = doc.source_file.code;
        var i: usize = 0;
        while (i + 1 < source.len) : (i += 1) {
            if (source[i] == '/' and source[i + 1] == '*') {
                const comment_start = i;
                i += 2;
                var nesting: usize = 1;
                while (i + 1 < source.len and nesting > 0) : (i += 1) {
                    if (source[i] == '/' and source[i + 1] == '*') {
                        nesting += 1;
                        i += 1;
                    } else if (source[i] == '*' and source[i + 1] == '/') {
                        nesting -= 1;
                        i += 1;
                    }
                }
                const comment_end = i;
                const start_line: u32 = @intCast(doc.source_file.lineAt(comment_start) -| 1);
                const end_line: u32 = @intCast(doc.source_file.lineAt(comment_end) -| 1);
                if (end_line > start_line) {
                    try ranges.append(arena, .{
                        .startLine = start_line,
                        .endLine = end_line,
                        .kind = .comment,
                    });
                }
            }
        }

        if (ranges.items.len == 0) return null;
        return ranges.items;
    }

    pub fn @"textDocument/selectionRange"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.SelectionRangeParams,
    ) !?[]const types.SelectionRange {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        var ranges: std.ArrayListUnmanaged(types.SelectionRange) = .empty;

        for (params.positions) |pos| {
            const offset = doc.positionToOffset(pos.line, pos.character) orelse continue;

            // Level 1: Token range under cursor
            var token_start = offset;
            var token_end = offset;

            if (offset < doc.src.len) {
                while (token_start > 0 and isIdentChar(doc.src[token_start - 1])) {
                    token_start -= 1;
                }
                while (token_end < doc.src.len and isIdentChar(doc.src[token_end])) {
                    token_end += 1;
                }
            }

            if (token_start == token_end and offset < doc.src.len) {
                token_end = offset + 1;
            }

            const level1_range = rangeForOffsets(doc.source_file, token_start, token_end);

            // Level 2: Expand to identifier chain (a.b.c)
            var chain_start = token_start;
            var chain_end = token_end;

            // Expand backwards: check for `.identifier` pattern
            while (chain_start > 1) {
                if (doc.src[chain_start - 1] == '.') {
                    const prev_end = chain_start - 1;
                    var prev_start = prev_end;
                    while (prev_start > 0 and isIdentChar(doc.src[prev_start - 1])) {
                        prev_start -= 1;
                    }
                    if (prev_start < prev_end) {
                        chain_start = prev_start;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Expand forwards: check for `.identifier` pattern
            while (chain_end < doc.src.len) {
                if (doc.src[chain_end] == '.') {
                    const next_start = chain_end + 1;
                    var next_end = next_start;
                    while (next_end < doc.src.len and isIdentChar(doc.src[next_end])) {
                        next_end += 1;
                    }
                    if (next_end > next_start) {
                        chain_end = next_end;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            const level2_range = rangeForOffsets(doc.source_file, chain_start, chain_end);

            // Level 3: Expand to enclosing delimiters (parentheses, brackets, braces)
            var delim_start = chain_start;
            var delim_end = chain_end;

            // Find enclosing delimiters by scanning backwards and forwards
            var depth_paren: i32 = 0;
            var depth_bracket: i32 = 0;
            var depth_brace: i32 = 0;

            // Scan backwards for opening delimiter
            var scan_back = chain_start;
            while (scan_back > 0) {
                scan_back -= 1;
                const c = doc.src[scan_back];
                switch (c) {
                    ')' => depth_paren += 1,
                    '(' => {
                        if (depth_paren == 0) {
                            delim_start = scan_back;
                            break;
                        }
                        depth_paren -= 1;
                    },
                    ']' => depth_bracket += 1,
                    '[' => {
                        if (depth_bracket == 0) {
                            delim_start = scan_back;
                            break;
                        }
                        depth_bracket -= 1;
                    },
                    '}' => depth_brace += 1,
                    '{' => {
                        if (depth_brace == 0) {
                            delim_start = scan_back;
                            break;
                        }
                        depth_brace -= 1;
                    },
                    else => {},
                }
            }

            // Only proceed with level 3 if we found an opener
            if (delim_start < chain_start) {
                const open_char = doc.src[delim_start];
                const close_char: u8 = switch (open_char) {
                    '(' => ')',
                    '[' => ']',
                    '{' => '}',
                    else => 0,
                };

                if (close_char != 0) {
                    var depth: i32 = 1;
                    var scan_fwd = delim_start + 1;
                    while (scan_fwd < doc.src.len and depth > 0) {
                        const c = doc.src[scan_fwd];
                        if (c == open_char) depth += 1;
                        if (c == close_char) depth -= 1;
                        scan_fwd += 1;
                    }
                    if (depth == 0) {
                        delim_end = scan_fwd;
                    }
                }
            }

            const level3_range = rangeForOffsets(doc.source_file, delim_start, delim_end);

            // Build parent chain: level3 -> level2 -> level1
            var level3: ?*types.SelectionRange = null;
            if (delim_start < chain_start or delim_end > chain_end) {
                const l3 = try arena.create(types.SelectionRange);
                l3.* = .{ .range = level3_range, .parent = null };
                level3 = l3;
            }

            var level2: ?*types.SelectionRange = level3;
            if (chain_start < token_start or chain_end > token_end) {
                const l2 = try arena.create(types.SelectionRange);
                l2.* = .{ .range = level2_range, .parent = level3 };
                level2 = l2;
            }

            try ranges.append(arena, .{ .range = level1_range, .parent = level2 });
        }

        if (ranges.items.len == 0) return null;
        return ranges.items;
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

    pub fn @"textDocument/signatureHelp"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.SignatureHelpParams,
    ) !?types.SignatureHelp {
        const doc = self.files.get(params.textDocument.uri) orelse return null;
        const target_offset = doc.positionToOffset(params.position.line, params.position.character) orelse return null;

        if (target_offset == 0) return null;

        // Use lexer to properly skip strings/comments
        var lexer = try wrenalyzer.Lexer.new(arena, doc.source_file);

        // Collect all tokens up to cursor position
        var tokens: std.ArrayListUnmanaged(Token) = .empty;
        while (true) {
            const token = try lexer.readToken();
            if (token.type == .eof) break;
            if (token.start >= target_offset) break;
            try tokens.append(arena, token);
        }

        if (tokens.items.len == 0) return null;

        // Find matching `(` by walking backwards through tokens
        var paren_depth: i32 = 0;
        var paren_idx: ?usize = null;
        var comma_count: u32 = 0;

        var i = tokens.items.len;
        while (i > 0) {
            i -= 1;
            const token = tokens.items[i];

            switch (token.type) {
                .rightParen => paren_depth += 1,
                .leftParen => {
                    if (paren_depth == 0) {
                        paren_idx = i;
                        break;
                    }
                    paren_depth -= 1;
                },
                .comma => {
                    if (paren_depth == 0) comma_count += 1;
                },
                else => {},
            }
        }

        const open_paren_idx = paren_idx orelse return null;
        if (open_paren_idx == 0) return null;

        // Find function name - token before `(`
        const name_token = tokens.items[open_paren_idx - 1];
        if (name_token.type != .name) return null;
        const func_name = name_token.name();

        // Check if there's a receiver (token before name is `.`)
        var receiver_name: ?[]const u8 = null;
        if (open_paren_idx >= 2) {
            const maybe_dot = tokens.items[open_paren_idx - 2];
            if (maybe_dot.type == .dot and open_paren_idx >= 3) {
                const recv_token = tokens.items[open_paren_idx - 3];
                if (recv_token.type == .name) {
                    receiver_name = recv_token.name();
                }
            }
        }

        // Try to find method in builtins or instance methods
        if (receiver_name) |recv| {
            // Check builtin class methods (e.g., System.print)
            if (Scope.BUILTIN_METHODS.get(recv)) |methods| {
                return self.buildSignatureHelp(arena, methods, func_name, comma_count);
            }

            // Check if receiver is a symbol with inferred type
            for (doc.symbols.items) |sym| {
                if (std.mem.eql(u8, sym.name, recv)) {
                    if (sym.inferred_type) |inferred| {
                        const type_name = @tagName(inferred);
                        if (Scope.INSTANCE_METHODS.get(type_name)) |methods| {
                            return self.buildSignatureHelp(arena, methods, func_name, comma_count);
                        }
                    }
                    break;
                }
            }
        }

        // Look up function in document symbols
        for (doc.symbols.items) |sym| {
            if (!std.mem.eql(u8, sym.name, func_name)) continue;
            if (sym.fn_arity) |arity| {
                var signatures: std.ArrayListUnmanaged(types.SignatureInformation) = .empty;
                var params_list: std.ArrayListUnmanaged(types.ParameterInformation) = .empty;

                for (0..arity) |_| {
                    try params_list.append(arena, .{
                        .label = .{ .string = "param" },
                    });
                }

                try signatures.append(arena, .{
                    .label = try std.fmt.allocPrint(arena, "{s}({d} args)", .{ func_name, arity }),
                    .parameters = params_list.items,
                });

                // Safe activeParameter calculation (avoid underflow)
                const active_param: u32 = if (arity == 0) 0 else @min(comma_count, @as(u32, @intCast(arity - 1)));

                return .{
                    .signatures = signatures.items,
                    .activeSignature = 0,
                    .activeParameter = active_param,
                };
            }
        }

        return null;
    }

    fn buildSignatureHelp(
        self: *Handler,
        arena: std.mem.Allocator,
        methods: []const Scope.BuiltinMethod,
        func_name: []const u8,
        comma_count: u32,
    ) !?types.SignatureHelp {
        _ = self;
        var signatures: std.ArrayListUnmanaged(types.SignatureInformation) = .empty;

        for (methods) |method| {
            if (!std.mem.eql(u8, method.name, func_name)) continue;

            var params_list: std.ArrayListUnmanaged(types.ParameterInformation) = .empty;
            const arity = countParens(method.signature);
            for (0..arity) |_| {
                try params_list.append(arena, .{
                    .label = .{ .string = "param" },
                });
            }

            try signatures.append(arena, .{
                .label = try arena.dupe(u8, method.signature),
                .parameters = params_list.items,
            });
        }

        if (signatures.items.len == 0) return null;

        return .{
            .signatures = signatures.items,
            .activeSignature = 0,
            .activeParameter = comma_count,
        };
    }

    pub fn @"textDocument/documentSymbol"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.DocumentSymbolParams,
    ) !lsp.ResultType("textDocument/documentSymbol") {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        var symbols: std.ArrayListUnmanaged(types.DocumentSymbol) = .empty;

        // Extract symbols from module AST
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        const range = tokenToRange(name_token);
                        const class_name = try arena.dupe(u8, name_token.name());

                        // Collect methods first
                        var method_children: std.ArrayListUnmanaged(types.DocumentSymbol) = .empty;
                        for (class_stmt.methods) |method_node| {
                            if (method_node == .Method) {
                                const method = method_node.Method;
                                if (method.name) |method_name| {
                                    const method_name_str = try arena.dupe(u8, method_name.name());
                                    const method_range = tokenToRange(method_name);
                                    try method_children.append(arena, .{
                                        .name = method_name_str,
                                        .kind = .Method,
                                        .range = method_range,
                                        .selectionRange = method_range,
                                    });
                                }
                            }
                        }

                        try symbols.append(arena, .{
                            .name = class_name,
                            .kind = .Class,
                            .range = range,
                            .selectionRange = range,
                            .children = method_children.items,
                        });
                    }
                },
                .VarStmt => |var_stmt| {
                    if (var_stmt.name) |name_token| {
                        const range = tokenToRange(name_token);
                        const var_name = try arena.dupe(u8, name_token.name());
                        try symbols.append(arena, .{
                            .name = var_name,
                            .kind = .Variable,
                            .range = range,
                            .selectionRange = range,
                        });
                    }
                },
                else => {},
            }
        }

        return .{ .array_of_DocumentSymbol = symbols.items };
    }

    pub fn @"textDocument/inlayHint"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.InlayHintParams,
    ) !lsp.ResultType("textDocument/inlayHint") {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        var hints: std.ArrayListUnmanaged(types.InlayHint) = .empty;

        // Add type hints for variables with inferred types
        for (doc.symbols.items) |sym| {
            if (sym.kind != .variable) continue;
            const inferred = sym.inferred_type orelse continue;

            // Skip unknown types
            if (inferred == .unknown) continue;

            // Position hint after the variable name
            const end_offset = sym.token.start + sym.token.length;
            const line: u32 = @intCast(doc.source_file.lineAt(end_offset) -| 1);
            const line_start = doc.source_file.lines[line];
            const character: u32 = @intCast(end_offset - line_start);

            const type_name = switch (inferred) {
                .num => "Num",
                .string => "String",
                .bool_type => "Bool",
                .null_type => "Null",
                .list => "List",
                .map => "Map",
                .range => "Range",
                .fn_type => "Fn",
                .fiber => "Fiber",
                .class_type => sym.class_name orelse "Class",
                .unknown => continue,
            };

            try hints.append(arena, .{
                .position = .{ .line = line, .character = character },
                .label = .{ .string = try std.fmt.allocPrint(arena, ": {s}", .{type_name}) },
                .kind = .Type,
                .paddingLeft = false,
                .paddingRight = true,
            });
        }

        if (hints.items.len == 0) return null;
        return hints.items;
    }

    pub fn onResponse(
        _: *Handler,
        _: std.mem.Allocator,
        _: lsp.JsonRPCMessage.Response,
    ) !void {}

    fn countParens(signature: []const u8) usize {
        const open_index = std.mem.indexOfScalar(u8, signature, '(') orelse return 0;
        const close_index = std.mem.indexOfScalarPos(u8, signature, open_index, ')') orelse return 0;
        if (close_index <= open_index + 1) return 0;

        var count: usize = 1;
        var i = open_index + 1;
        while (i < close_index) : (i += 1) {
            if (signature[i] == ',') count += 1;
        }
        return count;
    }

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

        var doc = try Document.init(
            self.gpa,
            new_text,
            language,
        );

        try self.checkImportPaths(&doc, uri);

        log.debug("document init", .{});

        const gop = try self.files.getOrPut(self.gpa, uri);
        errdefer _ = self.files.remove(uri);

        if (gop.found_existing) {
            gop.value_ptr.deinit(self.gpa);
        } else {
            gop.key_ptr.* = try self.gpa.dupe(u8, uri);
        }

        gop.value_ptr.* = doc;

        // Load imports and build import graph
        self.loadImportsForFile(arena, uri) catch |err| {
            log.debug("Failed to load imports for {s}: {s}", .{ uri, @errorName(err) });
        };

        const reporter_diags = doc.getDiagnostics();
        log.debug("document errors: {d}", .{reporter_diags.len});

        if (reporter_diags.len != 0) {
            const diags = try arena.alloc(types.Diagnostic, reporter_diags.len);

            for (reporter_diags, diags) |rdiag, *d| {
                log.debug("Error is {s}", .{rdiag.message});
                var validated = rdiag.message;
                if (!std.unicode.utf8ValidateSlice(validated)) {
                    log.err("Invalid diagnostic message bytes (len={d})", .{validated.len});
                    validated = "Invalid error message";
                }
                const message_src = if (validated.len == 0) "Unknown error" else validated;
                const message = try arena.dupe(u8, message_src);
                d.* = .{
                    .range = tokenToRange(rdiag.token),
                    .severity = switch (rdiag.severity) {
                        .@"error" => .Error,
                        .warning => .Warning,
                        .info => .Information,
                        .hint => .Hint,
                    },
                    .message = message,
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

fn rangeForOffsets(source_file: *const wrenalyzer.SourceFile, start: usize, end: usize) types.Range {
    const line_num = source_file.lineAt(start);
    const line: u32 = if (line_num > 0) @intCast(line_num - 1) else 0;

    const col_start = source_file.columnAt(start);
    const col_end = source_file.columnAt(end);

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
