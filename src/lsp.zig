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
                .referencesProvider = .{ .bool = true },
                .renameProvider = .{ .bool = true },
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

        if (try getImportPathCompletions(arena, doc, params.position, params.textDocument.uri)) |items| {
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
        return try listImportCompletions(arena, uri, prefix);
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
            return uri[prefix.len..];
        }
        return uri;
    }

    fn ensureWrenExtension(arena: std.mem.Allocator, path: []const u8) ![]const u8 {
        if (std.mem.endsWith(u8, path, ".wren")) return path;
        return std.fmt.allocPrint(arena, "{s}.wren", .{path});
    }

    fn checkImportPaths(self: *Handler, doc: *Document, uri: []const u8) !void {
        const base_path = uriToPath(uri);
        const base_dir = std.fs.path.dirname(base_path) orelse base_path;

        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const path_token = import_stmt.path orelse continue;
                    const raw_path = stripQuotes(path_token.name());
                    if (!isImportFilePath(raw_path)) continue;

                    const with_ext = try ensureWrenExtension(self.gpa, raw_path);
                    defer if (with_ext.ptr != raw_path.ptr) self.gpa.free(with_ext);

                    const full_path = if (std.fs.path.isAbsolute(with_ext))
                        with_ext
                    else
                        try std.fs.path.join(self.gpa, &.{ base_dir, with_ext });
                    defer if (full_path.ptr != with_ext.ptr) self.gpa.free(full_path);

                    if (std.fs.cwd().access(full_path, .{ .mode = .read_only })) |_| {
                        continue;
                    } else |err| switch (err) {
                        error.FileNotFound => {
                            var buf: [256]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "Cannot find import '{s}'", .{raw_path}) catch "Cannot find import";
                            doc.reporter.reportError(path_token, msg);
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }
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
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.ReferenceParams,
    ) !lsp.ResultType("textDocument/references") {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

        const sym = doc.findSymbolAtPosition(
            params.position.line,
            params.position.character,
        ) orelse return null;

        const include_decl = params.context.includeDeclaration;

        var locations: std.ArrayListUnmanaged(types.Location) = .empty;

        var lexer = try wrenalyzer.Lexer.new(arena, doc.source_file);
        while (true) {
            const token = try lexer.readToken();
            if (token.type == .eof) break;

            switch (token.type) {
                .name, .field, .staticField => {},
                else => continue,
            }

            if (!std.mem.eql(u8, token.name(), sym.name)) continue;

            if (!include_decl and token.start == sym.token.start and token.length == sym.token.length) {
                continue;
            }

            try locations.append(arena, .{
                .uri = params.textDocument.uri,
                .range = tokenToRange(token),
            });
        }

        return locations.items;
    }

    pub fn @"textDocument/rename"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.RenameParams,
    ) !?types.WorkspaceEdit {
        const doc = self.files.get(params.textDocument.uri) orelse return null;

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

fn rangeForOffsets(source_file: wrenalyzer.SourceFile, start: usize, end: usize) types.Range {
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
