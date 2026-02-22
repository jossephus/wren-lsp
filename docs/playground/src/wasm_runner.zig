const std = @import("std");
const c = @cImport({
    @cInclude("wren.h");
});

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

var output_buf: std.ArrayListUnmanaged(u8) = .empty;
var error_buf: std.ArrayListUnmanaged(u8) = .empty;
var source_buf: std.ArrayListUnmanaged(u8) = .empty;

fn writeFn(_: ?*c.WrenVM, text: [*c]const u8) callconv(.c) void {
    if (text) |t| {
        const slice = std.mem.span(t);
        output_buf.appendSlice(allocator, slice) catch {};
    }
}

fn errorFn(
    _: ?*c.WrenVM,
    err_type: c.WrenErrorType,
    module_name: [*c]const u8,
    line: c_int,
    msg: [*c]const u8,
) callconv(.c) void {
    const mod: []const u8 = if (module_name != null) std.mem.span(module_name) else "unknown";
    const message: []const u8 = if (msg != null) std.mem.span(msg) else "";

    if (err_type == c.WREN_ERROR_COMPILE) {
        const s = std.fmt.allocPrint(allocator, "[{s} line {d}] Error: {s}\n", .{ mod, line, message }) catch return;
        defer allocator.free(s);
        error_buf.appendSlice(allocator, s) catch {};
    } else if (err_type == c.WREN_ERROR_RUNTIME) {
        const s = std.fmt.allocPrint(allocator, "Runtime Error: {s}\n", .{message}) catch return;
        defer allocator.free(s);
        error_buf.appendSlice(allocator, s) catch {};
    } else if (err_type == c.WREN_ERROR_STACK_TRACE) {
        const s = std.fmt.allocPrint(allocator, "  [{s} line {d}] in {s}\n", .{ mod, line, message }) catch return;
        defer allocator.free(s);
        error_buf.appendSlice(allocator, s) catch {};
    }
}

export fn main() callconv(.c) c_int {
    return 0;
}

export fn allocSource(len: usize) [*]u8 {
    source_buf.clearRetainingCapacity();
    source_buf.resize(allocator, len + 1) catch @panic("OOM");
    source_buf.items[len] = 0;
    return source_buf.items.ptr;
}

export fn runCode() u32 {
    output_buf.clearRetainingCapacity();
    error_buf.clearRetainingCapacity();

    var config: c.WrenConfiguration = undefined;
    c.wrenInitConfiguration(&config);
    config.writeFn = &writeFn;
    config.errorFn = &errorFn;

    const vm = c.wrenNewVM(&config) orelse return 2;
    defer c.wrenFreeVM(vm);

    const result = c.wrenInterpret(vm, "main", source_buf.items.ptr);
    return @intCast(result);
}

export fn getOutputPtr() [*]const u8 {
    if (output_buf.items.len == 0) return @ptrFromInt(0);
    return output_buf.items.ptr;
}

export fn getOutputLen() usize {
    return output_buf.items.len;
}

export fn getErrorPtr() [*]const u8 {
    if (error_buf.items.len == 0) return @ptrFromInt(0);
    return error_buf.items.ptr;
}

export fn getErrorLen() usize {
    return error_buf.items.len;
}
