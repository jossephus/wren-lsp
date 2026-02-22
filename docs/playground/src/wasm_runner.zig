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
var dummy_byte: u8 = 0;
var virtual_files: std.StringHashMapUnmanaged([]const u8) = .empty;

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

fn isRelativePath(path: []const u8) bool {
     if (path.len < 2) return false;
     if (path[0] == '.' and (path[1] == '/' or path[1] == '.')) return true;
     return false;
 }

fn resolveModuleFn(
     _: ?*c.WrenVM,
     importer: [*c]const u8,
     module: [*c]const u8,
 ) callconv(.c) ?[*:0]const u8 {
     _ = importer;
     const mod = std.mem.span(module);
 
     if (!isRelativePath(mod)) {
         return module;
     }
 
     var resolved_path_buf: [256]u8 = undefined;
     var pos: usize = 0;
 
     // All playground modules are in file:///playground/ directory
     const playground_prefix = "file:///playground/";
     @memcpy(resolved_path_buf[pos..][0..playground_prefix.len], playground_prefix);
     pos += playground_prefix.len;
 
     if (std.mem.startsWith(u8, mod, "./")) {
         const remainder = mod[2..];
         const needed = pos + remainder.len + 5; // +5 for ".wren"
         if (needed > resolved_path_buf.len) return module;
 
         @memcpy(resolved_path_buf[pos..][0..remainder.len], remainder);
         pos += remainder.len;
 
         if (!std.mem.endsWith(u8, remainder, ".wren")) {
             @memcpy(resolved_path_buf[pos..][0..5], ".wren");
             pos += 5;
         }
     } else if (std.mem.startsWith(u8, mod, "../")) {
         // For now, just return module as relative imports from playground aren't supported
         return module;
     } else {
         return module;
     }
 
     const resolved = resolved_path_buf[0..pos];
     if (virtual_files.contains(resolved)) {
         const duped = allocator.dupeZ(u8, resolved) catch return null;
         return duped.ptr;
     }
     return module;
 }

fn loadModuleFn(
     vm: ?*c.WrenVM,
     module_name: [*c]const u8,
 ) callconv(.c) c.WrenLoadModuleResult {
     const name = std.mem.span(module_name);
     var result: c.WrenLoadModuleResult = .{ .source = null };
 
     if (virtual_files.get(name)) |source| {
         const duped = allocator.dupeZ(u8, source) catch return result;
         result.source = duped.ptr;
     }
 
     _ = vm;
     return result;
 }

export fn main() callconv(.c) c_int {
    // Ensure virtual file functions are exported by referencing them
    if (false) {
        _ = allocVirtualFilePath(0);
        _ = allocVirtualFileContent(0);
        setVirtualFile();
    }
    return 0;
}

export fn allocSource(len: usize) [*]u8 {
    source_buf.clearRetainingCapacity();
    source_buf.resize(allocator, len + 1) catch @panic("OOM");
    source_buf.items[len] = 0;
    return source_buf.items.ptr;
}

var temp_path_buf: std.ArrayListUnmanaged(u8) = .empty;
var temp_source_buf: std.ArrayListUnmanaged(u8) = .empty;

export fn allocVirtualFilePath(len: usize) [*]u8 {
    temp_path_buf.clearRetainingCapacity();
    temp_path_buf.resize(allocator, len) catch @panic("OOM");
    return temp_path_buf.items.ptr;
}

export fn allocVirtualFileContent(len: usize) [*]u8 {
    temp_source_buf.clearRetainingCapacity();
    temp_source_buf.resize(allocator, len) catch @panic("OOM");
    return temp_source_buf.items.ptr;
}

export fn setVirtualFile() void {
    if (temp_path_buf.items.len == 0) return;
    const duped_path = allocator.dupe(u8, temp_path_buf.items) catch return;
    const duped_source = allocator.dupe(u8, temp_source_buf.items) catch {
        allocator.free(duped_path);
        return;
    };
    virtual_files.put(allocator, duped_path, duped_source) catch {
        allocator.free(duped_path);
        allocator.free(duped_source);
    };
}

export fn runCode() u32 {
     output_buf.clearRetainingCapacity();
     error_buf.clearRetainingCapacity();
 
     var config: c.WrenConfiguration = undefined;
     c.wrenInitConfiguration(&config);
     config.writeFn = &writeFn;
     config.errorFn = &errorFn;
     config.resolveModuleFn = &resolveModuleFn;
     config.loadModuleFn = &loadModuleFn;
 
     const vm = c.wrenNewVM(&config) orelse return 2;
     defer c.wrenFreeVM(vm);
 
     const result = c.wrenInterpret(vm, "main", source_buf.items.ptr);
     return @intCast(result);
 }

export fn getOutputPtr() [*]const u8 {
    if (output_buf.items.len == 0) return @ptrCast(&dummy_byte);
    return output_buf.items.ptr;
}

export fn getOutputLen() usize {
    return output_buf.items.len;
}

export fn getErrorPtr() [*]const u8 {
    if (error_buf.items.len == 0) return @ptrCast(&dummy_byte);
    return error_buf.items.ptr;
}

export fn getErrorLen() usize {
    return error_buf.items.len;
}
