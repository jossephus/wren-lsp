//! WASM entry point that hosts the real LSP handler.
//!
//! The browser sends one JSON-RPC message per `call()`. We run the normal
//! `lsp.basic_server` loop against a transport that feeds that message and then
//! an injected `exit` notification, so the loop handles exactly one turn.

const std = @import("std");
const lsp = @import("lsp");
const lsp_handler = @import("lsp.zig");

const allocator = std.heap.wasm_allocator;

pub const std_options: std.Options = .{
    .logFn = noopLog,
};

fn noopLog(
    comptime _: std.log.Level,
    comptime _: @Type(.enum_literal),
    comptime _: []const u8,
    _: anytype,
) void {}

const exit_message = "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}";

const ReadPhase = enum {
    input,
    exit,
};

var read_phase: ReadPhase = .input;
var input_bytes: std.ArrayListUnmanaged(u8) = .empty;

var output_message_starts: std.ArrayListUnmanaged(usize) = .empty;
var output_message_bytes: std.ArrayListUnmanaged(u8) = .empty;

var transport: lsp.Transport = .{ .vtable = &transport_vtable };
const transport_vtable: lsp.Transport.VTable = .{
    .readJsonMessage = readJsonMessage,
    .writeJsonMessage = writeJsonMessage,
};

var handler: ?lsp_handler.Handler = null;

fn readJsonMessage(_: *lsp.Transport, a: std.mem.Allocator) lsp.Transport.ReadError![]u8 {
    switch (read_phase) {
        .input => {
            read_phase = .exit;
            return try a.dupe(u8, input_bytes.items);
        },
        .exit => {
            read_phase = .input;
            return try a.dupe(u8, exit_message);
        },
    }
}

fn writeJsonMessage(_: *lsp.Transport, json_message: []const u8) lsp.Transport.WriteError!void {
    output_message_starts.append(allocator, output_message_bytes.items.len) catch return error.NoSpaceLeft;
    output_message_bytes.appendSlice(allocator, json_message) catch return error.NoSpaceLeft;
}

export fn createServer() void {
    if (handler) |*h| h.deinit();
    handler = lsp_handler.Handler.init(allocator, &transport);
}

export fn allocMessage(len: usize) [*]const u8 {
    input_bytes.clearRetainingCapacity();
    input_bytes.resize(allocator, len) catch @panic("OOM");
    return input_bytes.items.ptr;
}

export fn call() void {
    if (handler == null) createServer();

    output_message_starts.clearRetainingCapacity();
    output_message_bytes.clearRetainingCapacity();
    read_phase = .input;

    lsp.basic_server.run(allocator, &transport, &handler.?, null) catch {};
}

export fn outputMessageCount() usize {
    return output_message_starts.items.len;
}

export fn outputMessagePtr(index: usize) [*]const u8 {
    return output_message_bytes.items[output_message_starts.items[index]..].ptr;
}

export fn outputMessageLen(index: usize) usize {
    const next_start = if (index + 1 < output_message_starts.items.len)
        output_message_starts.items[index + 1]
    else
        output_message_bytes.items.len;

    return next_start - output_message_starts.items[index];
}
