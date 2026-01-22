const std = @import("std");
const lsp = @import("lsp");
const lsp_handler = @import("lsp.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var read_buffer: [65536]u8 = undefined;
    var stdio = lsp.Transport.Stdio.init(
        &read_buffer,
        std.fs.File.stdin(),
        std.fs.File.stdout(),
    );

    var handler = lsp_handler.Handler.init(allocator, &stdio.transport);

    try lsp.basic_server.run(
        allocator,
        &stdio.transport,
        &handler,
        lsp_handler.log.err,
    );
}
