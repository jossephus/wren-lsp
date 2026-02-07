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
    /// Workspace folder paths for import graph scanning.
    workspace_folders: std.ArrayListUnmanaged([]const u8) = .empty,

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
            const old_src = entry.value_ptr.src;
            entry.value_ptr.deinit(self.gpa);
            self.gpa.free(old_src);
        }
        self.files.deinit(self.gpa);

        for (self.workspace_folders.items) |folder| {
            self.gpa.free(folder);
        }
        self.workspace_folders.deinit(self.gpa);
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

        // Store workspace folders for later scanning
        if (params.workspaceFolders) |folders| {
            for (folders) |folder| {
                const path = uriToPath(folder.uri);
                try self.workspace_folders.append(self.gpa, try self.gpa.dupe(u8, path));
                log.info("workspace folder: {s}", .{path});
            }
        } else if (params.rootUri) |root_uri| {
            const path = uriToPath(root_uri);
            try self.workspace_folders.append(self.gpa, try self.gpa.dupe(u8, path));
            log.info("workspace root: {s}", .{path});
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
        self: *Handler,
        arena: std.mem.Allocator,
        _: types.InitializedParams,
    ) !void {
        self.scanWorkspaceForImports(arena) catch |err| {
            log.warn("Failed to scan workspace: {s}", .{@errorName(err)});
        };
    }

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
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.DidSaveTextDocumentParams,
    ) !void {
        // When a module is saved, re-analyze files that import from it
        // so they get updated type information for imported symbols
        const importers = self.getImporters(params.textDocument.uri);
        for (importers) |importer_uri| {
            const importer_doc = self.files.get(importer_uri) orelse continue;
            // Re-analyze the importer with fresh import symbols
            const src_copy = try self.gpa.dupeZ(u8, importer_doc.src);
            self.loadFile(arena, src_copy, importer_uri, importer_doc.language) catch |err| {
                log.debug("Failed to re-analyze importer {s}: {s}", .{ importer_uri, @errorName(err) });
                self.gpa.free(src_copy);
            };
        }
    }

    pub fn @"textDocument/didClose"(
        self: *Handler,
        _: std.mem.Allocator,
        params: types.DidCloseTextDocumentParams,
    ) !void {
        var kv = self.files.fetchRemove(params.textDocument.uri) orelse return;
        self.gpa.free(kv.key);
        const old_src = kv.value.src;
        kv.value.deinit(self.gpa);
        self.gpa.free(old_src);
    }

    pub fn @"textDocument/completion"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.CompletionParams,
    ) !lsp.ResultType("textDocument/completion") {
        const doc = self.files.get(params.textDocument.uri) orelse {
            return .{ .CompletionList = .{ .isIncomplete = false, .items = &.{} } };
        };

        if (try self.getImportSymbolCompletions(arena, doc, params.position, params.textDocument.uri)) |items| {
            return .{ .CompletionList = .{ .isIncomplete = false, .items = items } };
        }

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

        if (try self.getAttributeCompletions(arena, doc, offset, uri)) |items| {
            return items;
        }

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
        return try self.listImportCompletions(arena, uri, prefix);
    }

    fn listImportCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        prefix: []const u8,
    ) !?[]types.CompletionItem {
        const config = self.getConfig(uri);
        const project_root = config.project_root orelse ".";
        const module_dirs = resolution.ModuleEntry.extractDirectories(arena, config.modules) catch &.{};

        const trimmed_prefix = if (std.mem.startsWith(u8, prefix, "./")) prefix[2..] else prefix;
        const needs_dot = std.mem.startsWith(u8, prefix, "./");

        var items: std.ArrayListUnmanaged(types.CompletionItem) = .empty;

        for (config.modules) |entry| {
            switch (entry) {
                .named => |named| {
                    if (prefix.len == 0 or std.mem.startsWith(u8, named.name, prefix)) {
                        try items.append(arena, .{
                            .label = named.name,
                            .kind = .Module,
                            .insertText = named.name,
                        });
                    }
                },
                .directory => {},
            }
        }

        // Scan configured module directories
        for (module_dirs) |mod_path| {
            const resolved_path = if (std.mem.startsWith(u8, mod_path, "./"))
                try std.fs.path.join(arena, &.{ project_root, mod_path[2..] })
            else if (!std.fs.path.isAbsolute(mod_path))
                try std.fs.path.join(arena, &.{ project_root, mod_path })
            else
                mod_path;

            var dir = std.fs.cwd().openDir(resolved_path, .{ .iterate = true }) catch continue;
            defer dir.close();

            var walker = try dir.walk(arena);
            defer walker.deinit();

            while (try walker.next()) |entry| {
                if (entry.kind != .file) continue;
                if (!std.mem.endsWith(u8, entry.path, ".wren")) continue;

                const path_no_ext = entry.path[0 .. entry.path.len - 5];
                if (trimmed_prefix.len > 0 and !std.mem.startsWith(u8, path_no_ext, trimmed_prefix)) continue;

                try items.append(arena, .{
                    .label = try arena.dupe(u8, path_no_ext),
                    .kind = .File,
                    .insertText = try arena.dupe(u8, path_no_ext),
                });
            }
        }

        // Also scan current file's directory for relative imports
        const base_path = uriToPath(uri);
        const base_dir = std.fs.path.dirname(base_path) orelse base_path;

        if (std.fs.cwd().openDir(base_dir, .{ .iterate = true })) |dir_val| {
            var dir = dir_val;
            defer dir.close();

            var walker = try dir.walk(arena);
            defer walker.deinit();

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
        } else |_| {}

        if (items.items.len == 0) return null;
        return items.items;
    }

    fn getImportSymbolCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        position: types.Position,
        uri: []const u8,
    ) !?[]types.CompletionItem {
        const line_slice = self.getLineSlice(doc, position) orelse return null;
        const local_offset = self.getLocalOffset(doc, position) orelse return null;

        const parsed = parseImportLine(line_slice) orelse return null;

        if (local_offset < parsed.symbols_start) return null;

        const symbols_before_cursor = line_slice[parsed.symbols_start..local_offset];
        const current_prefix = extractCurrentPrefix(symbols_before_cursor);
        const already_imported = collectAlreadyImported(arena, symbols_before_cursor) catch &.{};

        log.debug("import symbol completion: module='{s}' prefix='{s}' already_imported={d}", .{
            parsed.module_path,
            current_prefix,
            already_imported.len,
        });

        const module = try self.getImportedModule(arena, uri, parsed.module_path) orelse return null;
        return self.getModuleExportedSymbols(arena, module, current_prefix, already_imported);
    }

    const ParsedImport = struct {
        module_path: []const u8,
        symbols_start: usize,
    };

    fn parseImportLine(line: []const u8) ?ParsedImport {
        const import_idx = std.mem.indexOf(u8, line, "import") orelse return null;
        var i = import_idx + "import".len;

        while (i < line.len and std.ascii.isWhitespace(line[i])) : (i += 1) {}
        if (i >= line.len) return null;

        const quote = line[i];
        if (quote != '"' and quote != '\'') return null;

        const path_start = i + 1;
        const path_end = std.mem.indexOfScalarPos(u8, line, path_start, quote) orelse return null;
        const module_path = line[path_start..path_end];
        if (module_path.len == 0) return null;

        var j = path_end + 1;
        while (j < line.len and std.ascii.isWhitespace(line[j])) : (j += 1) {}

        if (j + 3 > line.len) return null;
        if (!std.mem.eql(u8, line[j .. j + 3], "for")) return null;

        return .{
            .module_path = module_path,
            .symbols_start = j + 3,
        };
    }

    fn extractCurrentPrefix(symbols_before_cursor: []const u8) []const u8 {
        const last_comma = std.mem.lastIndexOfScalar(u8, symbols_before_cursor, ',');
        const start = if (last_comma) |idx| idx + 1 else 0;
        return std.mem.trim(u8, symbols_before_cursor[start..], " \t");
    }

    fn collectAlreadyImported(arena: std.mem.Allocator, symbols_str: []const u8) ![]const []const u8 {
        var list: std.ArrayListUnmanaged([]const u8) = .empty;
        var iter = std.mem.splitScalar(u8, symbols_str, ',');
        while (iter.next()) |part| {
            const trimmed = std.mem.trim(u8, part, " \t");
            if (trimmed.len > 0) {
                try list.append(arena, trimmed);
            }
        }
        if (list.items.len > 0) {
            _ = list.pop();
        }
        return list.items;
    }

    fn getLineSlice(self: *Handler, doc: Document, position: types.Position) ?[]const u8 {
        _ = self;
        const lines = doc.source_file.lines;
        const line_index: usize = @intCast(position.line);
        if (line_index >= lines.len) return null;

        const line_start = lines[line_index];
        const line_end = if (line_index + 1 < lines.len) lines[line_index + 1] else doc.src.len;
        return doc.src[line_start..line_end];
    }

    fn getLocalOffset(self: *Handler, doc: Document, position: types.Position) ?usize {
        _ = self;
        const offset = doc.positionToOffset(position.line, position.character) orelse return null;
        const lines = doc.source_file.lines;
        const line_index: usize = @intCast(position.line);
        if (line_index >= lines.len) return null;
        return offset - lines[line_index];
    }

    fn getImportedModule(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        module_path: []const u8,
    ) !?wrenalyzer.Ast.Module {
        const import_uri = try self.resolveImportUri(arena, uri, module_path);

        if (self.files.get(import_uri) == null) {
            self.loadImportedFile(import_uri) catch |err| {
                log.debug("import symbol completion: failed loading '{s}' ({s})", .{ import_uri, @errorName(err) });
            };
        }

        if (self.files.get(import_uri)) |import_doc| {
            return import_doc.module;
        }

        const chain = self.getResolverChain(uri) catch return null;
        const config = self.getConfig(uri);
        const project_root = config.project_root orelse ".";
        const request = ResolveRequest{
            .importer_uri = uri,
            .import_string = module_path,
            .project_root = project_root,
        };

        if (chain.resolve(request)) |result| {
            if (result.source) |source| {
                const source_z = arena.dupeZ(u8, source) catch return null;
                const virtual_doc = Document.init(arena, source_z, .wren) catch return null;
                return virtual_doc.module;
            }
        }
        return null;
    }

    fn getModuleExportedSymbols(
        _: *Handler,
        arena: std.mem.Allocator,
        module: wrenalyzer.Ast.Module,
        prefix: []const u8,
        already_imported: []const []const u8,
    ) !?[]types.CompletionItem {
        var items: std.ArrayListUnmanaged(types.CompletionItem) = .empty;

        for (module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |s| {
                    if (s.name) |t| {
                        const name = t.name();
                        if (shouldIncludeSymbol(name, prefix, already_imported)) {
                            try items.append(arena, .{ .label = name, .kind = .Class });
                        }
                    }
                },
                .VarStmt => |s| {
                    if (s.name) |t| {
                        const name = t.name();
                        if (shouldIncludeSymbol(name, prefix, already_imported)) {
                            try items.append(arena, .{ .label = name, .kind = .Variable });
                        }
                    }
                },
                .ImportStmt => |import_stmt| {
                    // Imported symbols become global variables that can be re-exported
                    if (import_stmt.variables) |vars| {
                        for (vars) |var_token| {
                            if (var_token) |t| {
                                const name = t.name();
                                if (shouldIncludeSymbol(name, prefix, already_imported)) {
                                    try items.append(arena, .{ .label = name, .kind = .Variable });
                                }
                            }
                        }
                    }
                },
                else => {},
            }
        }

        if (items.items.len == 0) return null;
        return items.items;
    }

    fn shouldIncludeSymbol(name: []const u8, prefix: []const u8, already_imported: []const []const u8) bool {
        const matches_prefix = prefix.len == 0 or std.mem.startsWith(u8, name, prefix);
        const already_used = for (already_imported) |imported| {
            if (std.mem.eql(u8, imported, name)) break true;
        } else false;
        return matches_prefix and !already_used;
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

            // Follow the import chain to find the original class definition
            if (try self.resolveClassDefinition(arena, import_uri, receiver_name, 0)) |result| {
                return self.getClassStaticCompletionsInModule(arena, result.module, receiver_name);
            }
        }

        if (try self.findImportModuleByClassName(arena, doc, receiver_name, uri)) |module| {
            return self.getClassStaticCompletionsInModule(arena, module, receiver_name);
        }

        return null;
    }

    const SymbolDefinitionResult = struct {
        module: wrenalyzer.Ast.Module,
        uri: []const u8,
        is_class: bool,
    };

    /// Recursively follows import chains to find the original symbol definition.
    /// Returns the module containing the actual ClassStmt or VarStmt for the given symbol.
    fn resolveSymbolDefinition(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        symbol_name: []const u8,
        visited: *std.StringHashMapUnmanaged(void),
    ) !?SymbolDefinitionResult {
        // Cycle detection
        if (visited.contains(uri)) return null;
        try visited.put(arena, uri, {});

        // Safety limit
        if (visited.count() > 32) return null;

        if (self.files.get(uri) == null) {
            self.loadImportedFile(uri) catch |err| {
                log.debug("resolveSymbolDefinition: failed loading '{s}' ({s})", .{ uri, @errorName(err) });
                return null;
            };
        }

        const doc = self.files.get(uri) orelse return null;

        // Check if the symbol is defined directly in this module
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), symbol_name)) {
                            return .{ .module = doc.module, .uri = uri, .is_class = true };
                        }
                    }
                },
                .VarStmt => |var_stmt| {
                    if (var_stmt.name) |name_token| {
                        if (std.mem.eql(u8, name_token.name(), symbol_name)) {
                            return .{ .module = doc.module, .uri = uri, .is_class = false };
                        }
                    }
                },
                else => {},
            }
        }

        // Symbol not found directly, check if it's a re-exported import
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const variables = import_stmt.variables orelse continue;
                    const path_token = import_stmt.path orelse continue;

                    for (variables) |maybe_var| {
                        const var_token = maybe_var orelse continue;
                        if (std.mem.eql(u8, var_token.name(), symbol_name)) {
                            // Found the re-export, follow the chain
                            const import_path = stripQuotes(path_token.name());
                            const next_uri = try self.resolveImportUri(arena, uri, import_path);
                            return self.resolveSymbolDefinition(arena, next_uri, symbol_name, visited);
                        }
                    }
                },
                else => {},
            }
        }

        return null;
    }

    /// Convenience wrapper that creates a visited set for resolveSymbolDefinition.
    fn resolveClassDefinition(
        self: *Handler,
        arena: std.mem.Allocator,
        uri: []const u8,
        class_name: []const u8,
        _: usize,
    ) !?SymbolDefinitionResult {
        var visited: std.StringHashMapUnmanaged(void) = .empty;
        return self.resolveSymbolDefinition(arena, uri, class_name, &visited);
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

        // Follow the import chain to find the original class definition
        if (try self.resolveClassDefinition(arena, import_uri, class_name, 0)) |result| {
            return self.getClassInstanceCompletionsInModule(arena, result.module, class_name);
        }

        return null;
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

    const AttributeChain = struct {
        class_name: []const u8,
        depth: enum { attributes, self_level, methods_level },
    };

    fn parseAttributeChain(src: []const u8, dot_offset: usize) ?AttributeChain {
        var pos = dot_offset;

        var seg2_end = pos;
        while (seg2_end > 0 and isIdentChar(src[seg2_end - 1])) {
            seg2_end -= 1;
        }
        const seg2 = src[seg2_end..pos];

        if (seg2.len == 0) return null;

        if (seg2_end == 0 or src[seg2_end - 1] != '.') return null;
        pos = seg2_end - 1;

        if (std.mem.eql(u8, seg2, "self") or std.mem.eql(u8, seg2, "methods")) {
            var seg1_end = pos;
            while (seg1_end > 0 and isIdentChar(src[seg1_end - 1])) {
                seg1_end -= 1;
            }
            const seg1 = src[seg1_end..pos];
            if (!std.mem.eql(u8, seg1, "attributes")) return null;

            if (seg1_end == 0 or src[seg1_end - 1] != '.') return null;
            pos = seg1_end - 1;

            var class_end = pos;
            while (class_end > 0 and isIdentChar(src[class_end - 1])) {
                class_end -= 1;
            }
            const class_name = src[class_end..pos];
            if (class_name.len == 0) return null;

            return .{
                .class_name = class_name,
                .depth = if (std.mem.eql(u8, seg2, "self")) .self_level else .methods_level,
            };
        }

        if (std.mem.eql(u8, seg2, "attributes")) {
            var class_end = pos;
            while (class_end > 0 and isIdentChar(src[class_end - 1])) {
                class_end -= 1;
            }
            const class_name = src[class_end..pos];
            if (class_name.len == 0) return null;

            return .{ .class_name = class_name, .depth = .attributes };
        }

        return null;
    }

    fn getAttributeCompletions(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        dot_offset: usize,
        uri: []const u8,
    ) !?[]types.CompletionItem {
        const chain = parseAttributeChain(doc.src, dot_offset) orelse return null;

        log.debug("attribute completion: class='{s}' depth={s}", .{
            chain.class_name,
            @tagName(chain.depth),
        });

        const class_stmt = try self.findClassStmt(arena, doc, chain.class_name, uri) orelse return null;

        return switch (chain.depth) {
            .attributes => blk: {
                var items: std.ArrayListUnmanaged(types.CompletionItem) = .empty;
                if (hasRuntimeMeta(class_stmt.meta)) {
                    try items.append(arena, .{ .label = "self", .kind = .Property, .detail = "Class-level attributes" });
                }
                if (hasAnyMethodRuntimeMeta(class_stmt.methods)) {
                    try items.append(arena, .{ .label = "methods", .kind = .Property, .detail = "Method-level attributes" });
                }
                break :blk if (items.items.len > 0) items.items else null;
            },
            .self_level => self.getClassAttributeGroupCompletions(arena, class_stmt),
            .methods_level => self.getMethodAttributeSignatureCompletions(arena, class_stmt.methods),
        };
    }

    fn findClassStmt(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        class_name: []const u8,
        uri: []const u8,
    ) !?wrenalyzer.Ast.ClassStmt {
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |cs| {
                    if (cs.name) |n| {
                        if (std.mem.eql(u8, n.name(), class_name)) return cs;
                    }
                },
                else => {},
            }
        }

        if (self.findImportPath(doc, class_name)) |import_path| {
            const import_uri = try self.resolveImportUri(arena, uri, import_path);
            if (try self.resolveClassDefinition(arena, import_uri, class_name, 0)) |result| {
                for (result.module.statements) |stmt| {
                    switch (stmt) {
                        .ClassStmt => |cs| {
                            if (cs.name) |n| {
                                if (std.mem.eql(u8, n.name(), class_name)) return cs;
                            }
                        },
                        else => {},
                    }
                }
            }
        }

        return null;
    }

    fn hasRuntimeMeta(meta: ?*const wrenalyzer.Ast.Meta) bool {
        const m = meta orelse return false;
        for (m.attrs) |attr| {
            for (attr.occurrences) |occ| {
                if (occ.introducer.type == .hashBang) return true;
            }
        }
        return false;
    }

    fn hasAnyMethodRuntimeMeta(methods: []wrenalyzer.Ast.Node) bool {
        for (methods) |method_node| {
            switch (method_node) {
                .Method => |method| {
                    if (hasRuntimeMeta(method.meta)) return true;
                },
                else => {},
            }
        }
        return false;
    }

    fn getClassAttributeGroupCompletions(
        _: *Handler,
        arena: std.mem.Allocator,
        class_stmt: wrenalyzer.Ast.ClassStmt,
    ) !?[]types.CompletionItem {
        const meta = class_stmt.meta orelse return null;
        var items: std.ArrayListUnmanaged(types.CompletionItem) = .empty;
        var seen_ungrouped = false;

        for (meta.attrs) |attr| {
            var is_runtime = false;
            var has_group = false;
            var has_ungrouped = false;

            for (attr.occurrences) |occ| {
                if (occ.introducer.type != .hashBang) continue;
                is_runtime = true;
                switch (occ.value) {
                    .group => has_group = true,
                    .none, .expr => has_ungrouped = true,
                }
            }

            if (!is_runtime) continue;

            if (has_group) {
                try items.append(arena, .{
                    .label = attr.name_tok.name(),
                    .kind = .Property,
                    .detail = "Attribute group",
                });
            }
            if (has_ungrouped and !seen_ungrouped) {
                seen_ungrouped = true;
            }
        }

        if (items.items.len == 0 and !seen_ungrouped) return null;
        return items.items;
    }

    fn getMethodAttributeSignatureCompletions(
        _: *Handler,
        arena: std.mem.Allocator,
        methods: []wrenalyzer.Ast.Node,
    ) !?[]types.CompletionItem {
        var items: std.ArrayListUnmanaged(types.CompletionItem) = .empty;

        for (methods) |method_node| {
            switch (method_node) {
                .Method => |method| {
                    if (!hasRuntimeMeta(method.meta)) continue;
                    const name_tok = method.name orelse continue;
                    const sig = try formatMethodSignature(arena, method, name_tok.name());
                    try items.append(arena, .{
                        .label = sig,
                        .kind = .Method,
                        .detail = "Method with runtime attributes",
                    });
                },
                else => {},
            }
        }

        if (items.items.len == 0) return null;
        return items.items;
    }

    fn formatMethodSignature(
        arena: std.mem.Allocator,
        method: wrenalyzer.Ast.Method,
        name: []const u8,
    ) ![]const u8 {
        const prefix: []const u8 = if (method.foreignKeyword != null and method.staticKeyword != null)
            "foreign static "
        else if (method.staticKeyword != null)
            "static "
        else if (method.constructKeyword != null)
            ""
        else
            "";

        const arity = method.parameters.len;
        if (arity == 0) {
            return std.fmt.allocPrint(arena, "{s}{s}", .{ prefix, name });
        }

        var params_buf: std.ArrayListUnmanaged(u8) = .empty;
        try params_buf.appendSlice(arena, prefix);
        try params_buf.appendSlice(arena, name);
        try params_buf.append(arena, '(');
        for (0..arity) |i| {
            if (i > 0) try params_buf.append(arena, ',');
            try params_buf.append(arena, '_');
        }
        try params_buf.append(arena, ')');
        return params_buf.items;
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
            .import_string = import_path,
            .project_root = project_root,
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
        if (value.len >= 2) {
            const first = value[0];
            const last = value[value.len - 1];
            if ((first == '"' and last == '"') or (first == '\'' and last == '\'')) {
                return value[1 .. value.len - 1];
            }
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
                            .import_string = raw_path,
                            .project_root = project_root,
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
                                // For virtual modules with source, load and validate
                                if (result.source) |source| {
                                    self.loadVirtualModule(result.canonical_id, source) catch {};
                                    resolved_uri = self.gpa.dupe(u8, result.canonical_id) catch null;
                                }
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
                .ImportStmt => |import_stmt| {
                    // Imported symbols become global variables that can be re-exported
                    if (import_stmt.variables) |vars| {
                        for (vars) |var_token| {
                            if (var_token) |t| {
                                if (std.mem.eql(u8, t.name(), symbol_name)) return true;
                            }
                        }
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

        try self.loadModuleSource(uri, contents);
    }

    /// Load a virtual module from inline source content.
    fn loadVirtualModule(self: *Handler, canonical_id: []const u8, source: []const u8) !void {
        try self.loadModuleSource(canonical_id, source);
    }

    /// Common logic for loading a module from source content.
    fn loadModuleSource(self: *Handler, uri: []const u8, contents: []const u8) !void {
        const new_text = try self.gpa.alloc(u8, contents.len + 1);
        @memcpy(new_text[0..contents.len], contents);
        new_text[contents.len] = 0;

        const new_text_z: [:0]const u8 = new_text[0..contents.len :0];
        const doc = try Document.init(self.gpa, new_text_z, .wren);

        const gop = try self.files.getOrPut(self.gpa, uri);
        errdefer _ = self.files.remove(uri);

        if (gop.found_existing) {
            const old_src = gop.value_ptr.src;
            gop.value_ptr.deinit(self.gpa);
            self.gpa.free(old_src);
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

    /// Scan workspace folders for .wren files and build the import graph.
    /// Also scans external module directories referenced via `extends` in configs.
    fn scanWorkspaceForImports(self: *Handler, arena: std.mem.Allocator) !void {
        log.info("Scanning workspace for imports...", .{});

        const token = "wren-lsp/indexing";
        self.sendProgressBegin(token, "Indexing", "Discovering files...");

        var scanned_dirs: std.StringHashMapUnmanaged(void) = .empty;
        defer scanned_dirs.deinit(arena);

        var file_count: u32 = 0;
        for (self.workspace_folders.items) |folder| {
            try self.scanWrenFiles(arena, folder, &scanned_dirs, &file_count, token);

            try self.scanExternalModules(arena, folder, &scanned_dirs, &file_count, token);
        }

        const msg = std.fmt.allocPrint(arena, "Indexed {d} files", .{file_count}) catch "Indexing complete";
        self.sendProgressEnd(token, msg);
        log.info("Workspace scan complete. Loaded {d} files.", .{self.files.count()});
    }

    fn sendProgressBegin(self: *Handler, token: []const u8, title: []const u8, message: ?[]const u8) void {
        const ProgressValue = struct {
            kind: []const u8,
            title: []const u8,
            message: ?[]const u8 = null,
            cancellable: bool = false,
        };
        const ProgressParams = struct {
            token: []const u8,
            value: ProgressValue,
        };
        self.transport.writeNotification(
            self.gpa,
            "$/progress",
            ProgressParams,
            .{
                .token = token,
                .value = .{ .kind = "begin", .title = title, .message = message },
            },
            .{ .emit_null_optional_fields = false },
        ) catch {};
    }

    fn sendProgressReport(self: *Handler, token: []const u8, message: []const u8) void {
        const ProgressValue = struct {
            kind: []const u8,
            message: ?[]const u8 = null,
        };
        const ProgressParams = struct {
            token: []const u8,
            value: ProgressValue,
        };
        self.transport.writeNotification(
            self.gpa,
            "$/progress",
            ProgressParams,
            .{
                .token = token,
                .value = .{ .kind = "report", .message = message },
            },
            .{ .emit_null_optional_fields = false },
        ) catch {};
    }

    fn sendProgressEnd(self: *Handler, token: []const u8, message: ?[]const u8) void {
        const ProgressValue = struct {
            kind: []const u8,
            message: ?[]const u8 = null,
        };
        const ProgressParams = struct {
            token: []const u8,
            value: ProgressValue,
        };
        self.transport.writeNotification(
            self.gpa,
            "$/progress",
            ProgressParams,
            .{
                .token = token,
                .value = .{ .kind = "end", .message = message },
            },
            .{ .emit_null_optional_fields = false },
        ) catch {};
    }

    /// Scan external module directories referenced by configs in a directory.
    fn scanExternalModules(
        self: *Handler,
        arena: std.mem.Allocator,
        dir_path: []const u8,
        scanned_dirs: *std.StringHashMapUnmanaged(void),
        file_count: *u32,
        progress_token: []const u8,
    ) !void {
        const config_path = try std.fs.path.join(arena, &.{ dir_path, "wren-lsp.json" });

        if (std.fs.cwd().access(config_path, .{ .mode = .read_only })) |_| {
            const dummy_uri = try std.fmt.allocPrint(arena, "file://{s}/dummy.wren", .{dir_path});
            const config = self.config_loader.loadForFile(dummy_uri) catch return;
            const module_dirs = resolution.ModuleEntry.extractDirectories(arena, config.modules) catch &.{};

            for (module_dirs) |mod_dir| {
                if (!scanned_dirs.contains(mod_dir)) {
                    log.info("Scanning external module: {s}", .{mod_dir});
                    try self.scanWrenFiles(arena, mod_dir, scanned_dirs, file_count, progress_token);
                }
            }
        } else |_| {}

        // Recurse into subdirectories to find nested configs
        var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return;
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            if (entry.kind != .directory) continue;
            if (std.mem.startsWith(u8, entry.name, ".")) continue;
            if (std.mem.eql(u8, entry.name, "node_modules")) continue;

            const subdir = try std.fs.path.join(arena, &.{ dir_path, entry.name });
            try self.scanExternalModules(arena, subdir, scanned_dirs, file_count, progress_token);
        }
    }

    /// Recursively scan a directory for .wren files and load them.
    fn scanWrenFiles(
        self: *Handler,
        arena: std.mem.Allocator,
        dir_path: []const u8,
        scanned_dirs: *std.StringHashMapUnmanaged(void),
        file_count: *u32,
        progress_token: []const u8,
    ) !void {
        if (scanned_dirs.contains(dir_path)) return;
        try scanned_dirs.put(arena, dir_path, {});

        var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
            log.debug("Cannot open directory {s}: {s}", .{ dir_path, @errorName(err) });
            return;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (try iter.next()) |entry| {
            const full_path = try std.fs.path.join(arena, &.{ dir_path, entry.name });

            switch (entry.kind) {
                .file => {
                    if (std.mem.endsWith(u8, entry.name, ".wren")) {
                        const uri = try std.fmt.allocPrint(arena, "file://{s}", .{full_path});

                        if (self.files.get(uri) == null) {
                            self.loadImportedFile(uri) catch |err| {
                                log.debug("Failed to load {s}: {s}", .{ uri, @errorName(err) });
                                continue;
                            };

                            self.loadImportsForFile(arena, uri) catch |err| {
                                log.debug("Failed to load imports for {s}: {s}", .{ uri, @errorName(err) });
                            };

                            file_count.* += 1;
                            if (file_count.* % 10 == 0) {
                                const msg = std.fmt.allocPrint(arena, "{d} files indexed", .{file_count.*}) catch continue;
                                self.sendProgressReport(progress_token, msg);
                            }
                        }
                    }
                },
                .directory => {
                    if (std.mem.startsWith(u8, entry.name, ".")) continue;
                    if (std.mem.eql(u8, entry.name, "node_modules")) continue;
                    if (std.mem.eql(u8, entry.name, "zig-cache")) continue;
                    if (std.mem.eql(u8, entry.name, "zig-out")) continue;

                    try self.scanWrenFiles(arena, full_path, scanned_dirs, file_count, progress_token);
                },
                else => {},
            }
        }
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

        // Check if cursor is on a meta attribute
        if (try self.getMetaHover(arena, doc, params.position)) |hover| {
            return hover;
        }

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

        const content = try self.formatSymbolHover(arena, sym.name, sym.kind, sym.inferred_type, sym.fn_arity, sym.class_name);

        return .{
            .contents = .{ .MarkupContent = .{ .kind = .markdown, .value = content } },
            .range = range orelse tokenToRange(sym.token),
        };
    }

    fn formatSymbolHover(
        _: *Handler,
        arena: std.mem.Allocator,
        name: []const u8,
        kind: Scope.Symbol.Kind,
        inferred_type: ?Scope.Symbol.InferredType,
        fn_arity: ?usize,
        class_name: ?[]const u8,
    ) ![]const u8 {
        const kind_str = switch (kind) {
            .variable => "variable",
            .parameter => "parameter",
            .class => "class",
            .method => "method",
            .field => "field",
            .static_field => "static field",
            .import_var => "import",
        };

        const type_str: ?[]const u8 = if (inferred_type) |t| switch (t) {
            .num => "Num",
            .string => "String",
            .bool_type => "Bool",
            .null_type => "Null",
            .list => "List",
            .map => "Map",
            .range => "Range",
            .fn_type => "Fn",
            .class_type => class_name orelse "Class",
            .fiber => "Fiber",
            .unknown => null,
        } else null;

        if (type_str) |ts| {
            if (fn_arity) |arity| {
                return std.fmt.allocPrint(arena, "**{s}** `{s}`: {s} (arity {d})", .{ kind_str, name, ts, arity });
            }
            return std.fmt.allocPrint(arena, "**{s}** `{s}`: {s}", .{ kind_str, name, ts });
        }

        return std.fmt.allocPrint(arena, "**{s}** `{s}`", .{ kind_str, name });
    }

    const MetaContext = struct {
        owner_name: []const u8,
        owner_kind: []const u8,
    };

    fn getMetaHover(
        _: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        position: types.Position,
    ) !?types.Hover {
        const offset = doc.positionToOffset(position.line, position.character) orelse return null;

        const module = doc.module;

        if (module.meta) |meta| {
            if (try findMetaAtOffset(arena, meta, offset, .{ .owner_name = "(module)", .owner_kind = "module" })) |hover| {
                return hover;
            }
        }

        for (module.statements) |stmt| {
            if (try findMetaInStatement(arena, stmt, offset)) |hover| {
                return hover;
            }
        }

        return null;
    }

    fn findMetaInStatement(
        arena: std.mem.Allocator,
        stmt: wrenalyzer.Ast.Node,
        offset: usize,
    ) !?types.Hover {
        switch (stmt) {
            .ClassStmt => |class_stmt| {
                const class_name = if (class_stmt.name) |t| t.name() else "(anonymous)";
                if (class_stmt.meta) |meta| {
                    if (try findMetaAtOffset(arena, meta, offset, .{
                        .owner_name = class_name,
                        .owner_kind = "class",
                    })) |hover| return hover;
                }
                for (class_stmt.methods) |method_node| {
                    switch (method_node) {
                        .Method => |method| {
                            const method_name = if (method.name) |t| t.name() else "(anonymous)";
                            if (method.meta) |meta| {
                                if (try findMetaAtOffset(arena, meta, offset, .{
                                    .owner_name = method_name,
                                    .owner_kind = if (method.staticKeyword != null) "static method" else if (method.constructKeyword != null) "constructor" else "method",
                                })) |hover| return hover;
                            }
                        },
                        else => {},
                    }
                }
            },
            .VarStmt => |var_stmt| {
                const var_name = if (var_stmt.name) |t| t.name() else "(anonymous)";
                if (var_stmt.meta) |meta| {
                    if (try findMetaAtOffset(arena, meta, offset, .{
                        .owner_name = var_name,
                        .owner_kind = "variable",
                    })) |hover| return hover;
                }
            },
            else => {},
        }
        return null;
    }

    fn findMetaAtOffset(
        arena: std.mem.Allocator,
        meta: *const wrenalyzer.Ast.Meta,
        offset: usize,
        ctx: MetaContext,
    ) !?types.Hover {
        for (meta.attrs) |attr| {
            const name_tok = attr.name_tok;
            for (attr.occurrences) |occ| {
                const intro_start = occ.introducer.start;
                const attr_end = computeOccurrenceEnd(occ, name_tok);

                if (offset >= intro_start and offset < attr_end) {
                    const content = try formatMetaAttrHover(arena, attr, ctx);
                    return .{
                        .contents = .{ .MarkupContent = .{ .kind = .markdown, .value = content } },
                        .range = tokenToRange(name_tok),
                    };
                }
            }
        }
        return null;
    }

    fn computeOccurrenceEnd(occ: wrenalyzer.Ast.MetaOccurrence, name_tok: Token) usize {
        switch (occ.value) {
            .none => return name_tok.start + name_tok.length,
            .expr => |expr| {
                const val_tok = switch (expr) {
                    .BoolExpr => |_| name_tok,
                    .NumExpr => |n| n.value,
                    .StringExpr => |s| s.value,
                    else => name_tok,
                };
                return val_tok.start + val_tok.length;
            },
            .group => |group| {
                var end = name_tok.start + name_tok.length;
                for (group.items) |item| {
                    for (item.entries) |entry| {
                        const key_end = entry.key_tok.start + entry.key_tok.length;
                        if (key_end > end) end = key_end;
                        if (entry.value) |val| {
                            const val_end = switch (val) {
                                .NumExpr => |n| n.value.start + n.value.length,
                                .StringExpr => |s| s.value.start + s.value.length,
                                .BoolExpr => |_| key_end,
                                else => key_end,
                            };
                            if (val_end > end) end = val_end;
                        }
                    }
                }
                return end + 1;
            },
        }
    }

    fn formatMetaAttrHover(
        arena: std.mem.Allocator,
        attr: wrenalyzer.Ast.MetaAttr,
        ctx: MetaContext,
    ) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        const writer = buf.writer(arena);

        var is_runtime = false;
        for (attr.occurrences) |occ| {
            if (occ.introducer.type == .hashBang) {
                is_runtime = true;
                break;
            }
        }

        if (is_runtime) {
            try writer.writeAll("**#! attribute** (runtime)\n\n");
        } else {
            try writer.writeAll("**# attribute** (compile-time)\n\n");
        }

        try writer.print("**Name:** `{s}`\n\n", .{attr.name_tok.name()});
        try writer.print("**On:** {s} `{s}`\n\n", .{ ctx.owner_kind, ctx.owner_name });

        for (attr.occurrences) |occ| {
            const prefix: []const u8 = if (occ.introducer.type == .hashBang) "#!" else "#";
            switch (occ.value) {
                .none => {
                    try writer.print("```wren\n{s}{s}\n```\n", .{ prefix, attr.name_tok.name() });
                },
                .expr => |expr| {
                    const val_str = switch (expr) {
                        .StringExpr => |s| s.value.name(),
                        .NumExpr => |n| n.value.name(),
                        .BoolExpr => |b| if (b.value) "true" else "false",
                        else => "?",
                    };
                    try writer.print("```wren\n{s}{s} = {s}\n```\n", .{ prefix, attr.name_tok.name(), val_str });
                },
                .group => |group| {
                    try writer.print("```wren\n{s}{s}(", .{ prefix, attr.name_tok.name() });
                    var first = true;
                    for (group.items) |item| {
                        for (item.entries) |entry| {
                            if (!first) try writer.writeAll(", ");
                            first = false;
                            try writer.writeAll(entry.key_tok.name());
                            if (entry.value) |val| {
                                const val_str = switch (val) {
                                    .StringExpr => |s| s.value.name(),
                                    .NumExpr => |n| n.value.name(),
                                    .BoolExpr => |b| if (b.value) "true" else "false",
                                    else => "?",
                                };
                                try writer.print(" = {s}", .{val_str});
                            }
                        }
                    }
                    try writer.writeAll(")\n```\n");
                },
            }
        }

        return buf.items;
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

        // Check if cursor is on an import path - navigate to the imported file
        if (try self.findImportPathDefinition(arena, doc, params.textDocument.uri, params.position)) |result| {
            return result;
        }

        // Use resolved reference index if available
        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            log.info("definition: resolved symbol '{s}' kind={s}", .{ resolved.use_token.name(), @tagName(resolved.kind) });

            // Check if this is an imported symbol - if so, jump to the source module
            const symbol_name = resolved.use_token.name();
            if (self.getSymbolSourceModule(arena, params.textDocument.uri, symbol_name) != null) {
                log.info("definition: looking for source module of '{s}'", .{symbol_name});

                if (self.getSymbolSourceModule(arena, params.textDocument.uri, symbol_name)) |source_uri| {
                    log.info("definition: found source module '{s}'", .{source_uri});

                    // Follow the import chain to find the original definition
                    if (try self.resolveClassDefinition(arena, source_uri, symbol_name, 0)) |result| {
                        log.info("definition: resolved to original module '{s}'", .{result.uri});
                        if (self.files.get(result.uri)) |original_doc| {
                            if (self.findSymbolInModule(original_doc, symbol_name)) |source_token| {
                                log.info("definition: found symbol in original module", .{});
                                return .{
                                    .Definition = .{
                                        .Location = .{
                                            .uri = result.uri,
                                            .range = tokenToRange(source_token),
                                        },
                                    },
                                };
                            }
                        }
                    } else {
                        // Fallback to direct lookup if not a class (could be a variable)
                        if (self.files.get(source_uri) == null) {
                            self.loadImportedFile(source_uri) catch {};
                        }
                        if (self.files.get(source_uri)) |source_doc| {
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
                            }
                        }
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
                        // Follow the import chain to find the original class definition
                        if (try self.resolveClassDefinition(arena, source_uri.?, class_name.?, 0)) |result| {
                            log.info("definition: method - resolved class to original module '{s}'", .{result.uri});
                            if (self.files.get(result.uri)) |original_doc| {
                                if (findMethodInClassWithKind(original_doc, class_name.?, method_name, method_kind)) |method_token| {
                                    log.info("definition: found method in original module", .{});
                                    return .{
                                        .Definition = .{
                                            .Location = .{
                                                .uri = result.uri,
                                                .range = tokenToRange(method_token),
                                            },
                                        },
                                    };
                                }
                            }
                        }

                        // Fallback to direct lookup in immediate import
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
                        // Follow the import chain to find the original class definition
                        if (try self.resolveClassDefinition(arena, source_uri.?, class_name.?, 0)) |result| {
                            log.info("definition: field - resolved class to original module '{s}'", .{result.uri});
                            if (self.files.get(result.uri)) |original_doc| {
                                // Try field first
                                if (self.findFieldInClass(original_doc, class_name.?, field_name)) |field_token| {
                                    log.info("definition: found field in original module", .{});
                                    return .{
                                        .Definition = .{
                                            .Location = .{
                                                .uri = result.uri,
                                                .range = tokenToRange(field_token),
                                            },
                                        },
                                    };
                                }
                                // Try getter method (property)
                                if (findMethodInClassWithKind(original_doc, class_name.?, field_name, .instance)) |method_token| {
                                    log.info("definition: found property getter in original module", .{});
                                    return .{
                                        .Definition = .{
                                            .Location = .{
                                                .uri = result.uri,
                                                .range = tokenToRange(method_token),
                                            },
                                        },
                                    };
                                }
                            }
                        }

                        // Fallback to direct lookup in immediate import
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

        // Check for runtime meta attribute access: ClassName.attributes.self["key"]
        if (self.findMetaAttrAccessAtOffset(doc, offset)) |target| {
            log.info("definition: found meta attr access: class='{s}' method={s} key='{s}'", .{
                target.class_name,
                target.method_name orelse "(self)",
                target.key,
            });
            if (try self.resolveMetaAttrDefinition(arena, doc, params.textDocument.uri, target)) |result| {
                return result;
            }
        }

        // Check if cursor is on a #!key declaration
        if (findMetaAttrDeclAtOffset(doc.module, offset)) |target| {
            log.info("definition: cursor on meta attr declaration: class='{s}' key='{s}'", .{
                target.class_name,
                target.key,
            });
            return .{
                .Definition = .{
                    .Location = .{
                        .uri = params.textDocument.uri,
                        .range = tokenToRange(target.name_tok),
                    },
                },
            };
        }

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

    /// Check if cursor is on an import path and return definition to the imported file.
    fn findImportPathDefinition(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        uri: []const u8,
        position: types.Position,
    ) !?lsp.ResultType("textDocument/definition") {
        const offset = doc.positionToOffset(position.line, position.character) orelse return null;
        const raw_path = self.findImportPathAtOffset(doc, offset) orelse return null;

        log.info("definition: cursor on import path '{s}'", .{raw_path});

        const import_uri = self.resolveImportUri(arena, uri, raw_path) catch |err| {
            log.info("definition: failed to resolve import '{s}': {s}", .{ raw_path, @errorName(err) });
            return null;
        };

        if (!std.mem.startsWith(u8, import_uri, "file://")) {
            log.info("definition: import resolved to non-file URI '{s}'", .{import_uri});
            return null;
        }

        log.info("definition: navigating to import '{s}'", .{import_uri});

        return .{
            .Definition = .{
                .Location = .{
                    .uri = import_uri,
                    .range = .{
                        .start = .{ .line = 0, .character = 0 },
                        .end = .{ .line = 0, .character = 0 },
                    },
                },
            },
        };
    }

    /// Find import path token at offset, returns the unquoted path string.
    fn findImportPathAtOffset(_: *Handler, doc: Document, offset: usize) ?[]const u8 {
        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const path_token = import_stmt.path orelse continue;
                    if (offset >= path_token.start and offset < path_token.start + path_token.length) {
                        return stripQuotes(path_token.name());
                    }
                },
                else => {},
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

        // Check if cursor is on a runtime meta attribute declaration or usage
        const offset = doc.positionToOffset(params.position.line, params.position.character);
        if (offset) |off| {
            const meta_target = findMetaAttrDeclAtOffset(doc.module, off) orelse
                self.findMetaAttrAccessAtOffset(doc, off);
            if (meta_target) |target| {
                return self.findMetaAttrReferences(arena, doc, params.textDocument.uri, target, params.context.includeDeclaration);
            }
        }

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

    // --- Runtime meta attribute go-to-definition / find-references ---

    const MetaAttrTarget = struct {
        class_name: []const u8,
        method_name: ?[]const u8,
        key: []const u8,
        name_tok: Token,
    };

    fn findMetaAttrAccessAtOffset(self: *Handler, doc: Document, offset: usize) ?MetaAttrTarget {
        _ = self;
        return findMetaAttrAccessInNodes(doc.module.statements, offset);
    }

    fn findMetaAttrAccessInNodes(stmts: []const wrenalyzer.Ast.Node, offset: usize) ?MetaAttrTarget {
        for (stmts) |node| {
            if (findMetaAttrAccessInNode(node, offset)) |target| {
                return target;
            }
        }
        return null;
    }

    fn findMetaAttrAccessInNode(node: wrenalyzer.Ast.Node, offset: usize) ?MetaAttrTarget {
        switch (node) {
            .SubscriptExpr => |sub| {
                if (matchMetaAttrSubscript(sub, offset)) |target| {
                    return target;
                }
                if (findMetaAttrAccessInNode(sub.receiver.*, offset)) |t| return t;
                for (sub.arguments) |arg| {
                    if (findMetaAttrAccessInNode(arg, offset)) |t| return t;
                }
            },
            .CallExpr => |call| {
                if (call.receiver) |recv_ptr| {
                    if (findMetaAttrAccessInNode(recv_ptr.*, offset)) |t| return t;
                }
                for (call.arguments) |arg| {
                    if (findMetaAttrAccessInNode(arg, offset)) |t| return t;
                }
                if (call.blockArgument.*) |block| {
                    if (findMetaAttrAccessInNode(block, offset)) |t| return t;
                }
            },
            .VarStmt => |var_stmt| {
                if (var_stmt.initializer.*) |initializer| {
                    if (findMetaAttrAccessInNode(initializer, offset)) |t| return t;
                }
            },
            .ClassStmt => |class_stmt| {
                for (class_stmt.methods) |method_node| {
                    if (findMetaAttrAccessInNode(method_node, offset)) |t| return t;
                }
            },
            .Method => |method| {
                if (method.body.*) |body| {
                    if (findMetaAttrAccessInNode(body, offset)) |t| return t;
                }
            },
            .Body => |body| {
                if (body.expression) |expr_ptr| {
                    if (findMetaAttrAccessInNode(expr_ptr.*, offset)) |t| return t;
                }
                if (body.statements) |stmts| {
                    for (stmts) |stmt| {
                        if (findMetaAttrAccessInNode(stmt, offset)) |t| return t;
                    }
                }
            },
            .BlockStmt => |block| {
                for (block.statements) |stmt| {
                    if (findMetaAttrAccessInNode(stmt, offset)) |t| return t;
                }
            },
            .IfStmt => |if_stmt| {
                if (findMetaAttrAccessInNode(if_stmt.condition.*, offset)) |t| return t;
                if (findMetaAttrAccessInNode(if_stmt.thenBranch.*, offset)) |t| return t;
                if (if_stmt.elseBranch) |else_ptr| {
                    if (findMetaAttrAccessInNode(else_ptr.*, offset)) |t| return t;
                }
            },
            .WhileStmt => |while_stmt| {
                if (findMetaAttrAccessInNode(while_stmt.condition.*, offset)) |t| return t;
                if (findMetaAttrAccessInNode(while_stmt.body.*, offset)) |t| return t;
            },
            .ForStmt => |for_stmt| {
                if (findMetaAttrAccessInNode(for_stmt.iterator.*, offset)) |t| return t;
                if (findMetaAttrAccessInNode(for_stmt.body.*, offset)) |t| return t;
            },
            .ReturnStmt => |ret| {
                if (ret.value.*) |val| {
                    if (findMetaAttrAccessInNode(val, offset)) |t| return t;
                }
            },
            .InfixExpr => |infix| {
                if (findMetaAttrAccessInNode(infix.left.*, offset)) |t| return t;
                if (findMetaAttrAccessInNode(infix.right.*, offset)) |t| return t;
            },
            .PrefixExpr => |prefix| {
                if (findMetaAttrAccessInNode(prefix.right.*, offset)) |t| return t;
            },
            .AssignmentExpr => |assign| {
                if (findMetaAttrAccessInNode(assign.target.*, offset)) |t| return t;
                if (findMetaAttrAccessInNode(assign.value.*, offset)) |t| return t;
            },
            .GroupingExpr => |group| {
                if (findMetaAttrAccessInNode(group.expression.*, offset)) |t| return t;
            },
            .ListExpr => |list| {
                for (list.elements) |elem| {
                    if (findMetaAttrAccessInNode(elem, offset)) |t| return t;
                }
            },
            .MapExpr => |map| {
                for (map.entries) |entry| {
                    if (findMetaAttrAccessInNode(entry, offset)) |t| return t;
                }
            },
            else => {},
        }
        return null;
    }

    /// Match: SubscriptExpr where cursor is on the string key argument and
    /// receiver chain is ClassName.attributes.self or ClassName.attributes.methods[X]
    fn matchMetaAttrSubscript(sub: wrenalyzer.Ast.SubscriptExpr, offset: usize) ?MetaAttrTarget {
        if (sub.arguments.len != 1) return null;
        const arg = sub.arguments[0];
        const str_tok = switch (arg) {
            .StringExpr => |s| s.value,
            else => return null,
        };

        if (offset < str_tok.start or offset >= str_tok.start + str_tok.length) return null;

        const key = stripQuotes(str_tok.name());
        if (key.len == 0) return null;

        // Pattern 1: ClassName.attributes.self["key"]
        // receiver = CallExpr(name="self", receiver=CallExpr(name="attributes", receiver=CallExpr(name=ClassName)))
        if (matchAttrSelfChain(sub.receiver.*)) |class_name| {
            return .{
                .class_name = class_name,
                .method_name = null,
                .key = key,
                .name_tok = str_tok,
            };
        }

        // Pattern 2: ClassName.attributes.methods["methodName"]["key"]
        // receiver = SubscriptExpr(
        //   receiver = CallExpr(name="methods", receiver=CallExpr(name="attributes", receiver=CallExpr(name=ClassName))),
        //   args = [StringExpr("methodName")]
        // )
        if (matchAttrMethodsChain(sub.receiver.*)) |result| {
            return .{
                .class_name = result.class_name,
                .method_name = result.method_name,
                .key = key,
                .name_tok = str_tok,
            };
        }

        return null;
    }

    fn matchAttrSelfChain(node: wrenalyzer.Ast.Node) ?[]const u8 {
        // CallExpr(name="self", receiver=CallExpr(name="attributes", receiver=CallExpr(name=ClassName, receiver=null)))
        const self_call = switch (node) {
            .CallExpr => |c| c,
            else => return null,
        };
        if (!std.mem.eql(u8, self_call.name.name(), "self")) return null;
        const attr_ptr = self_call.receiver orelse return null;
        return matchAttrChainBase(attr_ptr.*);
    }

    const MethodChainResult = struct {
        class_name: []const u8,
        method_name: []const u8,
    };

    fn matchAttrMethodsChain(node: wrenalyzer.Ast.Node) ?MethodChainResult {
        // SubscriptExpr(
        //   receiver = CallExpr(name="methods", receiver=CallExpr(name="attributes", receiver=ClassName)),
        //   args = [StringExpr("methodName")]
        // )
        const inner_sub = switch (node) {
            .SubscriptExpr => |s| s,
            else => return null,
        };
        if (inner_sub.arguments.len != 1) return null;
        const method_str = switch (inner_sub.arguments[0]) {
            .StringExpr => |s| s.value,
            else => return null,
        };
        const method_name = stripQuotes(method_str.name());
        if (method_name.len == 0) return null;

        const methods_call = switch (inner_sub.receiver.*) {
            .CallExpr => |c| c,
            else => return null,
        };
        if (!std.mem.eql(u8, methods_call.name.name(), "methods")) return null;
        const attr_ptr = methods_call.receiver orelse return null;
        const class_name = matchAttrChainBase(attr_ptr.*) orelse return null;
        return .{ .class_name = class_name, .method_name = method_name };
    }

    fn matchAttrChainBase(node: wrenalyzer.Ast.Node) ?[]const u8 {
        // CallExpr(name="attributes", receiver=CallExpr(name=ClassName, receiver=null))
        const attr_call = switch (node) {
            .CallExpr => |c| c,
            else => return null,
        };
        if (!std.mem.eql(u8, attr_call.name.name(), "attributes")) return null;
        const class_ptr = attr_call.receiver orelse return null;
        const class_call = switch (class_ptr.*) {
            .CallExpr => |c| c,
            else => return null,
        };
        if (class_call.receiver != null) return null;
        return class_call.name.name();
    }

    /// Find a #! meta attribute declaration at the given offset.
    fn findMetaAttrDeclAtOffset(module: wrenalyzer.Ast.Module, offset: usize) ?MetaAttrTarget {
        for (module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    const class_name = if (class_stmt.name) |t| t.name() else continue;
                    // Class-level meta
                    if (class_stmt.meta) |meta| {
                        if (findRuntimeAttrAtOffset(meta, offset)) |attr| {
                            return .{
                                .class_name = class_name,
                                .method_name = null,
                                .key = attr.name_tok.name(),
                                .name_tok = attr.name_tok,
                            };
                        }
                    }
                    // Method-level meta
                    for (class_stmt.methods) |method_node| {
                        switch (method_node) {
                            .Method => |method| {
                                if (method.meta) |meta| {
                                    if (findRuntimeAttrAtOffset(meta, offset)) |attr| {
                                        return .{
                                            .class_name = class_name,
                                            .method_name = if (method.name) |t| t.name() else null,
                                            .key = attr.name_tok.name(),
                                            .name_tok = attr.name_tok,
                                        };
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

    fn findRuntimeAttrAtOffset(meta: *const wrenalyzer.Ast.Meta, offset: usize) ?wrenalyzer.Ast.MetaAttr {
        for (meta.attrs) |attr| {
            for (attr.occurrences) |occ| {
                if (occ.introducer.type != .hashBang) continue;
                const intro_start = occ.introducer.start;
                const attr_end = attr.name_tok.start + attr.name_tok.length;
                if (offset >= intro_start and offset < attr_end) {
                    return attr;
                }
            }
        }
        return null;
    }

    fn resolveMetaAttrDefinition(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        current_uri: []const u8,
        target: MetaAttrTarget,
    ) !?lsp.ResultType("textDocument/definition") {
        // Find the class definition (possibly in another module)
        var def_uri: []const u8 = current_uri;
        var def_module: wrenalyzer.Ast.Module = doc.module;

        if (self.getSymbolSourceModule(arena, current_uri, target.class_name)) |source_uri| {
            if (try self.resolveClassDefinition(arena, source_uri, target.class_name, 0)) |result| {
                def_uri = result.uri;
                def_module = result.module;
            }
        }

        // Search the ClassStmt in the target module
        for (def_module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    const name = if (class_stmt.name) |t| t.name() else continue;
                    if (!std.mem.eql(u8, name, target.class_name)) continue;

                    if (target.method_name == null) {
                        // Class-level attribute
                        if (class_stmt.meta) |meta| {
                            if (findRuntimeAttrByKey(meta, target.key)) |attr_tok| {
                                return .{
                                    .Definition = .{
                                        .Location = .{
                                            .uri = def_uri,
                                            .range = tokenToRange(attr_tok),
                                        },
                                    },
                                };
                            }
                        }
                    } else {
                        // Method-level attribute
                        for (class_stmt.methods) |method_node| {
                            switch (method_node) {
                                .Method => |method| {
                                    const mname = if (method.name) |t| t.name() else continue;
                                    if (!std.mem.eql(u8, mname, target.method_name.?)) continue;
                                    if (method.meta) |meta| {
                                        if (findRuntimeAttrByKey(meta, target.key)) |attr_tok| {
                                            return .{
                                                .Definition = .{
                                                    .Location = .{
                                                        .uri = def_uri,
                                                        .range = tokenToRange(attr_tok),
                                                    },
                                                },
                                            };
                                        }
                                    }
                                },
                                else => {},
                            }
                        }
                    }
                },
                else => {},
            }
        }
        return null;
    }

    fn findRuntimeAttrByKey(meta: *const wrenalyzer.Ast.Meta, key: []const u8) ?Token {
        for (meta.attrs) |attr| {
            if (!std.mem.eql(u8, attr.name_tok.name(), key)) continue;
            for (attr.occurrences) |occ| {
                if (occ.introducer.type == .hashBang) {
                    return attr.name_tok;
                }
            }
        }
        return null;
    }

    fn findMetaAttrReferences(
        self: *Handler,
        arena: std.mem.Allocator,
        doc: Document,
        current_uri: []const u8,
        target: MetaAttrTarget,
        include_decl: bool,
    ) !?[]types.Location {
        var locations: std.ArrayListUnmanaged(types.Location) = .empty;

        // Find declaration location
        var decl_uri: []const u8 = current_uri;
        var decl_module: wrenalyzer.Ast.Module = doc.module;

        if (self.getSymbolSourceModule(arena, current_uri, target.class_name)) |source_uri| {
            if (try self.resolveClassDefinition(arena, source_uri, target.class_name, 0)) |result| {
                decl_uri = result.uri;
                decl_module = result.module;
            }
        }

        if (include_decl) {
            // Add the declaration itself
            for (decl_module.statements) |stmt| {
                switch (stmt) {
                    .ClassStmt => |class_stmt| {
                        const name = if (class_stmt.name) |t| t.name() else continue;
                        if (!std.mem.eql(u8, name, target.class_name)) continue;

                        if (target.method_name == null) {
                            if (class_stmt.meta) |meta| {
                                if (findRuntimeAttrByKey(meta, target.key)) |attr_tok| {
                                    try locations.append(arena, .{
                                        .uri = decl_uri,
                                        .range = tokenToRange(attr_tok),
                                    });
                                }
                            }
                        } else {
                            for (class_stmt.methods) |method_node| {
                                switch (method_node) {
                                    .Method => |method| {
                                        const mname = if (method.name) |t| t.name() else continue;
                                        if (!std.mem.eql(u8, mname, target.method_name.?)) continue;
                                        if (method.meta) |meta| {
                                            if (findRuntimeAttrByKey(meta, target.key)) |attr_tok| {
                                                try locations.append(arena, .{
                                                    .uri = decl_uri,
                                                    .range = tokenToRange(attr_tok),
                                                });
                                            }
                                        }
                                    },
                                    else => {},
                                }
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        // Search all loaded files for usages
        var file_iter = self.files.iterator();
        while (file_iter.next()) |entry| {
            const file_uri = entry.key_ptr.*;
            const file_doc = entry.value_ptr.*;
            self.collectMetaAttrUsages(arena, &locations, file_uri, file_doc.module, target);
        }

        if (locations.items.len == 0) return null;
        return locations.items;
    }

    fn collectMetaAttrUsages(
        _: *Handler,
        arena: std.mem.Allocator,
        locations: *std.ArrayListUnmanaged(types.Location),
        uri: []const u8,
        module: wrenalyzer.Ast.Module,
        target: MetaAttrTarget,
    ) void {
        for (module.statements) |stmt| {
            collectMetaAttrUsagesInNode(arena, locations, uri, stmt, target);
        }
    }

    fn collectMetaAttrUsagesInNode(
        arena: std.mem.Allocator,
        locations: *std.ArrayListUnmanaged(types.Location),
        uri: []const u8,
        node: wrenalyzer.Ast.Node,
        target: MetaAttrTarget,
    ) void {
        switch (node) {
            .SubscriptExpr => |sub| {
                if (matchMetaAttrSubscriptForTarget(sub, target)) |str_tok| {
                    locations.append(arena, .{
                        .uri = uri,
                        .range = tokenToRange(str_tok),
                    }) catch {};
                }
                collectMetaAttrUsagesInNode(arena, locations, uri, sub.receiver.*, target);
                for (sub.arguments) |arg| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, arg, target);
                }
            },
            .CallExpr => |call| {
                if (call.receiver) |recv_ptr| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, recv_ptr.*, target);
                }
                for (call.arguments) |arg| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, arg, target);
                }
                if (call.blockArgument.*) |block| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, block, target);
                }
            },
            .VarStmt => |var_stmt| {
                if (var_stmt.initializer.*) |initializer| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, initializer, target);
                }
            },
            .ClassStmt => |class_stmt| {
                for (class_stmt.methods) |method_node| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, method_node, target);
                }
            },
            .Method => |method| {
                if (method.body.*) |body| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, body, target);
                }
            },
            .Body => |body| {
                if (body.expression) |expr_ptr| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, expr_ptr.*, target);
                }
                if (body.statements) |stmts| {
                    for (stmts) |stmt| {
                        collectMetaAttrUsagesInNode(arena, locations, uri, stmt, target);
                    }
                }
            },
            .BlockStmt => |block| {
                for (block.statements) |stmt| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, stmt, target);
                }
            },
            .IfStmt => |if_stmt| {
                collectMetaAttrUsagesInNode(arena, locations, uri, if_stmt.condition.*, target);
                collectMetaAttrUsagesInNode(arena, locations, uri, if_stmt.thenBranch.*, target);
                if (if_stmt.elseBranch) |else_ptr| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, else_ptr.*, target);
                }
            },
            .WhileStmt => |while_stmt| {
                collectMetaAttrUsagesInNode(arena, locations, uri, while_stmt.condition.*, target);
                collectMetaAttrUsagesInNode(arena, locations, uri, while_stmt.body.*, target);
            },
            .ForStmt => |for_stmt| {
                collectMetaAttrUsagesInNode(arena, locations, uri, for_stmt.iterator.*, target);
                collectMetaAttrUsagesInNode(arena, locations, uri, for_stmt.body.*, target);
            },
            .ReturnStmt => |ret| {
                if (ret.value.*) |val| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, val, target);
                }
            },
            .InfixExpr => |infix| {
                collectMetaAttrUsagesInNode(arena, locations, uri, infix.left.*, target);
                collectMetaAttrUsagesInNode(arena, locations, uri, infix.right.*, target);
            },
            .PrefixExpr => |prefix| {
                collectMetaAttrUsagesInNode(arena, locations, uri, prefix.right.*, target);
            },
            .AssignmentExpr => |assign| {
                collectMetaAttrUsagesInNode(arena, locations, uri, assign.target.*, target);
                collectMetaAttrUsagesInNode(arena, locations, uri, assign.value.*, target);
            },
            .GroupingExpr => |group| {
                collectMetaAttrUsagesInNode(arena, locations, uri, group.expression.*, target);
            },
            .ListExpr => |list| {
                for (list.elements) |elem| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, elem, target);
                }
            },
            .MapExpr => |map| {
                for (map.entries) |entry| {
                    collectMetaAttrUsagesInNode(arena, locations, uri, entry, target);
                }
            },
            else => {},
        }
    }

    fn matchMetaAttrSubscriptForTarget(sub: wrenalyzer.Ast.SubscriptExpr, target: MetaAttrTarget) ?Token {
        if (sub.arguments.len != 1) return null;
        const arg = sub.arguments[0];
        const str_tok = switch (arg) {
            .StringExpr => |s| s.value,
            else => return null,
        };
        const key = stripQuotes(str_tok.name());
        if (!std.mem.eql(u8, key, target.key)) return null;

        if (target.method_name == null) {
            const class_name = matchAttrSelfChain(sub.receiver.*) orelse return null;
            if (!std.mem.eql(u8, class_name, target.class_name)) return null;
            return str_tok;
        } else {
            const result = matchAttrMethodsChain(sub.receiver.*) orelse return null;
            if (!std.mem.eql(u8, result.class_name, target.class_name)) return null;
            if (!std.mem.eql(u8, result.method_name, target.method_name.?)) return null;
            return str_tok;
        }
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

    /// Add rename edits for all files that import a symbol from a source module.
    fn addCrossFileRenameEdits(
        self: *Handler,
        arena: std.mem.Allocator,
        changes: *lsp.parser.Map(types.DocumentUri, []const types.TextEdit),
        importers: []const []const u8,
        target_name: []const u8,
        source_uri: []const u8,
        new_name: []const u8,
    ) !void {
        for (importers) |importer_uri| {
            const importer_doc = self.files.get(importer_uri) orelse continue;

            if (!self.fileImportsSymbol(importer_doc, target_name, source_uri)) {
                continue;
            }

            var importer_edits: std.ArrayListUnmanaged(types.TextEdit) = .empty;

            for (importer_doc.refs.items) |ref| {
                if (std.mem.eql(u8, ref.use_token.name(), target_name)) {
                    if (ref.kind == .import_var or ref.kind == .class) {
                        try importer_edits.append(arena, .{
                            .range = tokenToRange(ref.use_token),
                            .newText = new_name,
                        });
                    }
                }
            }

            for (importer_doc.symbols.items) |sym| {
                if (std.mem.eql(u8, sym.name, target_name) and sym.kind == .import_var) {
                    try importer_edits.append(arena, .{
                        .range = tokenToRange(sym.token),
                        .newText = new_name,
                    });
                }
            }

            if (importer_edits.items.len > 0) {
                const importer_uri_edits = try arena.alloc(types.TextEdit, importer_edits.items.len);
                @memcpy(importer_uri_edits, importer_edits.items);
                try changes.map.put(arena, importer_uri, importer_uri_edits);
            }
        }
    }

    pub fn @"textDocument/rename"(
        self: *Handler,
        arena: std.mem.Allocator,
        params: types.RenameParams,
    ) !?types.WorkspaceEdit {
        log.debug("rename: uri={s} line={d} col={d} newName={s}", .{ params.textDocument.uri, params.position.line, params.position.character, params.newName });
        const doc = self.files.get(params.textDocument.uri) orelse {
            log.debug("rename: document not found", .{});
            return null;
        };

        // Use resolved reference index if available
        if (doc.resolvedAtPosition(params.position.line, params.position.character)) |resolved| {
            log.debug("rename: resolved symbol found", .{});
            var edits: std.ArrayListUnmanaged(types.TextEdit) = .empty;

            // Track which token starts we've already added to avoid duplicates
            var seen_starts: std.AutoHashMapUnmanaged(usize, void) = .{};

            // Find all references to this declaration
            for (doc.refs.items) |ref| {
                if (ref.decl_token.start == resolved.decl_token.start) {
                    const gop = try seen_starts.getOrPut(arena, ref.use_token.start);
                    if (!gop.found_existing) {
                        try edits.append(arena, .{
                            .range = tokenToRange(ref.use_token),
                            .newText = params.newName,
                        });
                    }
                }
            }

            // Also include the declaration itself (check doc.symbols for module-level)
            for (doc.symbols.items) |sym| {
                if (sym.token.start == resolved.decl_token.start) {
                    const gop = try seen_starts.getOrPut(arena, sym.token.start);
                    if (!gop.found_existing) {
                        try edits.append(arena, .{
                            .range = tokenToRange(sym.token),
                            .newText = params.newName,
                        });
                    }
                    break;
                }
            }

            // If declaration not found yet (e.g., fields or local variables inside methods),
            // add it directly from the resolved ref's decl_token
            {
                const gop = try seen_starts.getOrPut(arena, resolved.decl_token.start);
                if (!gop.found_existing) {
                    try edits.append(arena, .{
                        .range = tokenToRange(resolved.decl_token),
                        .newText = params.newName,
                    });
                }
            }

            if (edits.items.len == 0) return null;

            const uri = params.textDocument.uri;
            const uri_edits = try arena.alloc(types.TextEdit, edits.items.len);
            @memcpy(uri_edits, edits.items);

            var changes = lsp.parser.Map(types.DocumentUri, []const types.TextEdit){};
            try changes.map.put(arena, uri, uri_edits);

            // Cross-file rename: handle classes, methods, and imported symbols
            const target_name = resolved.use_token.name();
            const symbol_kind = resolved.kind;
            log.debug("rename: target_name={s} symbol_kind={s}", .{ target_name, @tagName(symbol_kind) });

            if (symbol_kind == .class or symbol_kind == .method) {
                // Renaming from the source module - update all importers
                const importers = self.getImporters(params.textDocument.uri);
                log.debug("rename: found {d} importers for {s}", .{ importers.len, params.textDocument.uri });
                try self.addCrossFileRenameEdits(arena, &changes, importers, target_name, params.textDocument.uri, params.newName);
            } else if (symbol_kind == .import_var) {
                // Renaming an imported symbol - find source module and update there + all importers
                if (self.getSymbolSourceModule(arena, params.textDocument.uri, target_name)) |source_uri| {
                    log.debug("rename: import_var source module is {s}", .{source_uri});

                    // Add edits for the source module (the class definition)
                    if (self.files.get(source_uri)) |source_doc| {
                        var source_edits: std.ArrayListUnmanaged(types.TextEdit) = .empty;

                        // Find the class/method declaration
                        for (source_doc.symbols.items) |sym| {
                            if (std.mem.eql(u8, sym.name, target_name) and sym.kind == .class) {
                                try source_edits.append(arena, .{
                                    .range = tokenToRange(sym.token),
                                    .newText = params.newName,
                                });
                            }
                        }

                        // Find all refs in source module
                        for (source_doc.refs.items) |ref| {
                            if (std.mem.eql(u8, ref.use_token.name(), target_name)) {
                                try source_edits.append(arena, .{
                                    .range = tokenToRange(ref.use_token),
                                    .newText = params.newName,
                                });
                            }
                        }

                        if (source_edits.items.len > 0) {
                            const source_uri_edits = try arena.alloc(types.TextEdit, source_edits.items.len);
                            @memcpy(source_uri_edits, source_edits.items);
                            try changes.map.put(arena, source_uri, source_uri_edits);
                        }
                    }

                    // Update all other files that import this symbol from the source
                    const importers = self.getImporters(source_uri);
                    log.debug("rename: found {d} importers for source {s}", .{ importers.len, source_uri });
                    try self.addCrossFileRenameEdits(arena, &changes, importers, target_name, source_uri, params.newName);
                }
            }

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

        // Cross-file rename for declarations: if this is a class, update all importers
        if (sym.kind == .class) {
            const importers = self.getImporters(params.textDocument.uri);
            log.debug("rename (fallback): class '{s}' has {d} importers", .{ sym.name, importers.len });
            try self.addCrossFileRenameEdits(arena, &changes, importers, sym.name, params.textDocument.uri, params.newName);
        }

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

        const MetaState = enum { none, attr_name, attr_value, group_key, group_value };
        var meta_state: MetaState = .none;

        var lexer = try wrenalyzer.Lexer.new(arena, doc.source_file);

        while (true) {
            const token = try lexer.readToken();
            if (token.type == .eof) break;

            const token_type: types.SemanticTokenTypes = blk: {
                switch (meta_state) {
                    .none => {
                        if (token.type == .hash or token.type == .hashBang) {
                            meta_state = .attr_name;
                            break :blk .decorator;
                        }
                    },
                    .attr_name => {
                        if (token.type == .name) {
                            meta_state = .attr_value;
                            break :blk .decorator;
                        }
                        meta_state = .none;
                    },
                    .attr_value => {
                        if (token.type == .equal) {
                            break :blk .operator;
                        } else if (token.type == .leftParen) {
                            meta_state = .group_key;
                            break :blk .operator;
                        } else if (token.type == .hash or token.type == .hashBang) {
                            meta_state = .attr_name;
                            break :blk .decorator;
                        } else if (token.type == .line) {
                            meta_state = .none;
                        } else {
                            meta_state = .none;
                        }
                    },
                    .group_key => {
                        if (token.type == .name) {
                            meta_state = .group_value;
                            break :blk .property;
                        } else if (token.type == .rightParen) {
                            meta_state = .none;
                            break :blk .operator;
                        } else if (token.type == .comma) {
                            break :blk .operator;
                        } else if (token.type == .line) {
                            // Groups can span multiple lines
                        }
                    },
                    .group_value => {
                        if (token.type == .equal) {
                            break :blk .operator;
                        } else if (token.type == .comma) {
                            meta_state = .group_key;
                            break :blk .operator;
                        } else if (token.type == .rightParen) {
                            meta_state = .none;
                            break :blk .operator;
                        } else if (token.type == .line) {
                            // Groups can span multiple lines; stay in group_key state
                            meta_state = .group_key;
                        }
                    },
                }
                break :blk tokenTagToSemanticType(token.type) orelse continue;
            };

            const start_line_num = doc.source_file.lineAt(token.start);
            const start_line: u32 = if (start_line_num > 0) @intCast(start_line_num - 1) else 0;
            const start_col_num = doc.source_file.columnAt(token.start);
            const start_col: u32 = if (start_col_num > 0) @intCast(start_col_num - 1) else 0;

            const can_be_multiline = token.type == .string or token.type == .interpolation;

            if (can_be_multiline) {
                const end_offset = token.start + token.length;
                const token_text = doc.source_file.code[token.start..end_offset];
                var line_start: usize = 0;
                var current_line = start_line;
                var is_multiline = false;

                for (token_text, 0..) |ch, i| {
                    if (ch == '\n') {
                        is_multiline = true;
                        const seg_len: u32 = @intCast(i - line_start);
                        const seg_col: u32 = if (current_line == start_line) start_col else 0;

                        const delta_line = current_line - prev_line;
                        const delta_col = if (delta_line == 0) seg_col - prev_col else seg_col;

                        try data.append(arena, delta_line);
                        try data.append(arena, delta_col);
                        try data.append(arena, seg_len);
                        try data.append(arena, @intFromEnum(token_type));
                        try data.append(arena, 0);

                        prev_line = current_line;
                        prev_col = seg_col;
                        current_line += 1;
                        line_start = i + 1;
                    }
                }

                if (is_multiline) {
                    if (line_start < token_text.len) {
                        const seg_len: u32 = @intCast(token_text.len - line_start);
                        const seg_col: u32 = 0;

                        const delta_line = current_line - prev_line;
                        const delta_col = if (delta_line == 0) seg_col - prev_col else seg_col;

                        try data.append(arena, delta_line);
                        try data.append(arena, delta_col);
                        try data.append(arena, seg_len);
                        try data.append(arena, @intFromEnum(token_type));
                        try data.append(arena, 0);

                        prev_line = current_line;
                        prev_col = seg_col;
                    }
                    continue;
                }
            }

            const delta_line = start_line - prev_line;
            const delta_col = if (delta_line == 0) start_col - prev_col else start_col;

            try data.append(arena, delta_line);
            try data.append(arena, delta_col);
            try data.append(arena, @intCast(token.length));
            try data.append(arena, @intFromEnum(token_type));
            try data.append(arena, 0);

            prev_line = start_line;
            prev_col = start_col;
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

        for (doc.module.statements) |stmt| {
            switch (stmt) {
                .ClassStmt => |class_stmt| {
                    if (class_stmt.name) |name_token| {
                        const range = tokenToRange(name_token);
                        const class_name = try arena.dupe(u8, name_token.name());

                        var method_children: std.ArrayListUnmanaged(types.DocumentSymbol) = .empty;
                        for (class_stmt.methods) |method_node| {
                            if (method_node == .Method) {
                                const method = method_node.Method;
                                if (method.name) |method_name| {
                                    const method_name_str = try arena.dupe(u8, method_name.name());
                                    const method_range = tokenToRange(method_name);
                                    try method_children.append(arena, .{
                                        .name = method_name_str,
                                        .detail = formatMeta(arena, method.meta),
                                        .kind = .Method,
                                        .range = method_range,
                                        .selectionRange = method_range,
                                    });
                                }
                            }
                        }

                        try symbols.append(arena, .{
                            .name = class_name,
                            .detail = formatMeta(arena, class_stmt.meta),
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
                            .detail = formatMeta(arena, var_stmt.meta),
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

    fn formatMeta(arena: std.mem.Allocator, meta: ?*const wrenalyzer.Ast.Meta) ?[]const u8 {
        const m = meta orelse return null;
        if (m.isEmpty()) return null;

        var parts: std.ArrayListUnmanaged([]const u8) = .empty;
        for (m.attrs) |attr| {
            const attr_name = attr.name_tok.name();
            for (attr.occurrences) |occ| {
                const prefix: []const u8 = if (occ.introducer.type == .hashBang) "#!" else "#";
                const part = switch (occ.value) {
                    .none => std.fmt.allocPrint(arena, "{s}{s}", .{ prefix, attr_name }) catch continue,
                    .expr => |expr| blk: {
                        const val = exprToString(expr);
                        break :blk std.fmt.allocPrint(arena, "{s}{s} = {s}", .{ prefix, attr_name, val }) catch continue;
                    },
                    .group => |group| blk: {
                        var entries: std.ArrayListUnmanaged([]const u8) = .empty;
                        for (group.items) |item| {
                            for (item.entries) |entry| {
                                const key = entry.key_tok.name();
                                if (entry.value) |v| {
                                    const val = exprToString(v);
                                    entries.append(arena, std.fmt.allocPrint(arena, "{s} = {s}", .{ key, val }) catch continue) catch continue;
                                } else {
                                    entries.append(arena, key) catch continue;
                                }
                            }
                        }
                        const joined = std.mem.join(arena, ", ", entries.items) catch continue;
                        break :blk std.fmt.allocPrint(arena, "{s}{s}({s})", .{ prefix, attr_name, joined }) catch continue;
                    },
                };
                parts.append(arena, part) catch continue;
            }
        }

        if (parts.items.len == 0) return null;
        return std.mem.join(arena, " ", parts.items) catch null;
    }

    fn exprToString(node: wrenalyzer.Ast.Node) []const u8 {
        return switch (node) {
            .StringExpr => |s| s.value.name(),
            .NumExpr => |n| n.value.name(),
            .BoolExpr => |b| if (b.value) "true" else "false",
            .NullExpr => "null",
            else => "?",
        };
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

        // Build import symbols table from already-loaded imported modules
        var import_symbols: std.StringHashMapUnmanaged(Document.ImportSymbolInfo) = .empty;
        defer import_symbols.deinit(arena);

        try self.buildImportSymbols(arena, new_text, uri, &import_symbols);

        var doc = try Document.initWithImportSymbols(
            self.gpa,
            new_text,
            language,
            if (import_symbols.count() > 0) &import_symbols else null,
        );

        try self.checkImportPaths(&doc, uri);

        log.debug("document init", .{});

        const gop = try self.files.getOrPut(self.gpa, uri);
        errdefer _ = self.files.remove(uri);

        if (gop.found_existing) {
            const old_src = gop.value_ptr.src;
            gop.value_ptr.deinit(self.gpa);
            self.gpa.free(old_src);
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

    /// Build import symbols table by parsing imports from source, loading imported modules,
    /// and collecting their exports.
    fn buildImportSymbols(
        self: *Handler,
        arena: std.mem.Allocator,
        src: []const u8,
        uri: []const u8,
        import_symbols: *std.StringHashMapUnmanaged(Document.ImportSymbolInfo),
    ) !void {
        // Quick parse to extract imports without full resolution
        const source_file = try self.gpa.create(wrenalyzer.SourceFile);
        source_file.* = try wrenalyzer.SourceFile.new(self.gpa, "temp.wren", src);
        defer {
            source_file.deinit();
            self.gpa.destroy(source_file);
        }

        var reporter = wrenalyzer.Reporter.init(self.gpa);
        defer reporter.deinit();

        const lexer = try wrenalyzer.Lexer.new(self.gpa, source_file);
        var parser = try wrenalyzer.Parser.newWithReporter(self.gpa, lexer, &reporter);
        const module = try parser.parseModule();

        // Process each import statement
        for (module.statements) |stmt| {
            switch (stmt) {
                .ImportStmt => |import_stmt| {
                    const path_token = import_stmt.path orelse continue;
                    const raw_path = stripQuotes(path_token.name());

                    const import_uri = self.resolveImportUri(arena, uri, raw_path) catch continue;

                    // Load the imported module if not already loaded
                    if (self.files.get(import_uri) == null) {
                        if (std.mem.startsWith(u8, import_uri, "file://")) {
                            self.loadImportedFile(import_uri) catch continue;
                        }
                    }

                    // Get exports from imported module
                    const imported_doc = self.files.get(import_uri) orelse continue;

                    // For each imported variable, look up its type in the source module
                    if (import_stmt.variables) |vars| {
                        for (vars) |maybe_var| {
                            const var_token = maybe_var orelse continue;
                            const name = var_token.name();

                            if (imported_doc.exports.get(name)) |export_info| {
                                // Deep copy methods to arena so we don't hold pointers into source doc
                                const methods_copy: ?[]const Document.ClassMethodInfo = if (export_info.methods) |m|
                                    arena.dupe(Document.ClassMethodInfo, m) catch null
                                else
                                    null;

                                try import_symbols.put(arena, name, .{
                                    .kind = export_info.kind,
                                    .inferred_type = export_info.inferred_type,
                                    .fn_arity = export_info.fn_arity,
                                    .class_name = export_info.class_name,
                                    .methods = methods_copy,
                                });
                                log.debug("buildImportSymbols: '{s}' -> kind={s} type={s} arity={any} methods={any}", .{
                                    name,
                                    @tagName(export_info.kind),
                                    if (export_info.inferred_type) |t| @tagName(t) else "null",
                                    export_info.fn_arity,
                                    methods_copy != null,
                                });
                            }
                        }
                    }
                },
                else => {},
            }
        }
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
