const std = @import("std");
const lsp = @import("lsp");
const lsp_namespace = @import("../lsp.zig");
const Handler = lsp_namespace.Handler;
const getRange = lsp_namespace.getRange;
const Document = @import("Document.zig");

const log = std.log.scoped(.wren_lsp);

pub fn loadFile(
    self: *Handler,
    arena: std.mem.Allocator,
    new_text: [:0]const u8,
    uri: []const u8,
    language: lsp_namespace.Language,
) !void {
    errdefer @panic("error while loading document!");

    var res: lsp.types.PublishDiagnosticsParams = .{
        .uri = uri,
        .diagnostics = &.{},
    };

    const doc = try Document.init(
        self.gpa,
        new_text,
        language,
    );

    log.debug("document init changed", .{});

    const gop = try self.files.getOrPut(self.gpa, uri);
    errdefer _ = self.files.remove(uri);

    if (gop.found_existing) {
        gop.value_ptr.deinit(self.gpa);
    } else {
        gop.key_ptr.* = try self.gpa.dupe(u8, uri);
    }

    gop.value_ptr.* = doc;

    log.debug("document init changed {d}", .{doc.parser.errors.items.len});

    if (doc.parser.errors.items.len != 0) {
        const diags = try arena.alloc(lsp.types.Diagnostic, doc.parser.errors.items.len);

        for (doc.parser.errors.items, diags) |err, *d| {
            log.debug("Error is {s}", .{err.message});
            const range = getRange(err.token);
            d.* = .{
                .range = range,
                .severity = .Error,
                .message = err.message,
            };
        }

        res.diagnostics = diags;
    }
    //} else {
    //if (doc.super_ast) |super_ast| {
    //const diags = try arena.alloc(
    //lsp.types.Diagnostic,
    //super_ast.errors.len,
    //);
    //for (super_ast.errors, diags) |err, *d| {
    //const range = getRange(err.main_location, doc.src);
    //d.* = .{
    //.range = range,
    //.severity = .Error,
    //.message = @tagName(err.kind),
    //};
    //}
    //res.diagnostics = diags;
    //}
    //}

    const msg = try self.server.sendToClientNotification(
        "textDocument/publishDiagnostics",
        res,
    );

    defer self.gpa.free(msg);
}
