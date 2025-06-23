const std = @import("std");
const builtin = @import("builtin");
const lsp_namespace = @import("lsp.zig");
const lsp = @import("lsp");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const allocator = gpa.allocator();

    defer {
        _ = gpa.deinit();
    }

    var server: lsp_namespace.WrenLsp = undefined;

    var transport = lsp.Transport.init(
        std.io.getStdIn().reader(),
        std.io.getStdOut().writer(),
    );

    var handler: lsp_namespace.Handler = .{
        .gpa = allocator,
        .server = &server,
    };
    server = try lsp_namespace.WrenLsp.init(allocator, &transport, &handler);

    try server.loop();

    //const allocator = switch (builtin.cpu.arch) {
    //.wasm32 => std.heap.wasm_allocator,
    //else => general_purpose_allocator.allocator(),
    //};

    //var transport = Transport.init(
    //std.io.getStdIn().reader(),
    //std.io.getStdOut().writer(),
    //);
    //transport.message_tracing = false;

    //const server = try Server.init(allocator, &transport);
    //defer server.destroy();

    //try server.loop();

    //if (server.status == .exiting_failure) {
    //if (builtin.mode == .Debug) {
    //// make sure that GeneralPurposeAllocator.deinit gets run to detect leaks
    //return;
    //} else {
    //std.process.exit(1);
    //}
    //}
}
