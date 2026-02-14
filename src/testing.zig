const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;
const Handler = @import("lsp.zig").Handler;

pub const TestContext = struct {
    handler: Handler,
    arena: std.heap.ArenaAllocator,

    const test_uri = "file:///test/main.wren";

    pub fn init() TestContext {
        const allocator = std.testing.allocator;
        var ctx: TestContext = .{
            .handler = Handler.init(allocator, &noop_transport),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
        _ = ctx.handler.initialize(ctx.arena.allocator(), .{
            .capabilities = .{},
        }) catch {};
        return ctx;
    }

    pub fn deinit(self: *TestContext) void {
        self.handler.deinit();
        self.arena.deinit();
    }

    /// Open a virtual document with the given source.
    pub fn openDocument(self: *TestContext, source: []const u8) !void {
        try self.openDocumentWithUri(test_uri, source);
    }

    pub fn openDocumentWithUri(self: *TestContext, uri: []const u8, source: []const u8) !void {
        try self.handler.@"textDocument/didOpen"(self.arena.allocator(), .{
            .textDocument = .{
                .uri = uri,
                .languageId = "wren",
                .version = 1,
                .text = source,
            },
        });
    }

    /// Get completions at (line, character).
    pub fn completion(self: *TestContext, line: u32, character: u32) !lsp.ResultType("textDocument/completion") {
        return self.completionAt(test_uri, line, character);
    }

    pub fn completionAt(self: *TestContext, uri: []const u8, line: u32, character: u32) !lsp.ResultType("textDocument/completion") {
        return self.handler.@"textDocument/completion"(self.arena.allocator(), .{
            .textDocument = .{ .uri = uri },
            .position = .{ .line = line, .character = character },
        });
    }

    pub fn hover(self: *TestContext, line: u32, character: u32) !?types.Hover {
        return self.handler.@"textDocument/hover"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
            .position = .{ .line = line, .character = character },
        });
    }

    pub fn definition(self: *TestContext, line: u32, character: u32) !lsp.ResultType("textDocument/definition") {
        return self.handler.@"textDocument/definition"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
            .position = .{ .line = line, .character = character },
        });
    }

    pub fn references(self: *TestContext, line: u32, character: u32) !lsp.ResultType("textDocument/references") {
        return self.handler.@"textDocument/references"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
            .position = .{ .line = line, .character = character },
            .context = .{ .includeDeclaration = true },
        });
    }

    pub fn rename(self: *TestContext, line: u32, character: u32, new_name: []const u8) !?types.WorkspaceEdit {
        return self.handler.@"textDocument/rename"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
            .position = .{ .line = line, .character = character },
            .newName = new_name,
        });
    }

    pub fn documentSymbols(self: *TestContext) !lsp.ResultType("textDocument/documentSymbol") {
        return self.handler.@"textDocument/documentSymbol"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
        });
    }

    pub fn semanticTokens(self: *TestContext) !?types.SemanticTokens {
        return self.handler.@"textDocument/semanticTokens/full"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
        });
    }

    pub fn foldingRanges(self: *TestContext) !?[]const types.FoldingRange {
        return self.handler.@"textDocument/foldingRange"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
        });
    }

    pub fn documentHighlights(self: *TestContext, line: u32, character: u32) !?[]const types.DocumentHighlight {
        return self.handler.@"textDocument/documentHighlight"(self.arena.allocator(), .{
            .textDocument = .{ .uri = test_uri },
            .position = .{ .line = line, .character = character },
        });
    }

    /// Check if a label exists in a completion result.
    pub fn hasCompletionLabel(result: lsp.ResultType("textDocument/completion"), label: []const u8) bool {
        const unwrapped = result orelse return false;
        const items: ?[]const types.CompletionItem = switch (unwrapped) {
            .CompletionList => |list| list.items,
            .array_of_CompletionItem => |arr| arr,
        };
        if (items) |list| {
            for (list) |item| {
                if (std.mem.eql(u8, item.label, label)) return true;
            }
        }
        return false;
    }
};

// No-op transport for testing — Handler uses transport for progress notifications
// which we can safely discard.
var noop_transport: lsp.Transport = .{ .vtable = &noop_vtable };

const noop_vtable: lsp.Transport.VTable = .{
    .readJsonMessage = &noopRead,
    .writeJsonMessage = &noopWrite,
};

fn noopRead(_: *lsp.Transport, _: std.mem.Allocator) lsp.Transport.ReadError![]u8 {
    return error.EndOfStream;
}

fn noopWrite(_: *lsp.Transport, _: []const u8) lsp.Transport.WriteError!void {}
