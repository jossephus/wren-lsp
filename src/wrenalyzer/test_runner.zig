const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const SourceFile = @import("source_file.zig");
const fs = std.fs;

const TestResult = struct {
    path: []const u8,
    success: bool,
    error_msg: ?[]const u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const test_dirs = [_][]const u8{ "test/language", "test/core", "test/limit" };

    var total: usize = 0;
    var passed: usize = 0;
    var failed: usize = 0;
    var failed_files: std.ArrayListUnmanaged(TestResult) = .empty;
    defer failed_files.deinit(allocator);

    for (test_dirs) |dir| {
        var walker = try fs.cwd().openDir(dir, .{ .iterate = true });
        defer walker.close();

        var iter = walker.iterate();
        try collectAndTest(allocator, dir, &iter, &total, &passed, &failed, &failed_files);
    }

    std.debug.print("\n=== Test Results ===\n", .{});
    std.debug.print("Total: {d}, Passed: {d}, Failed: {d}\n", .{ total, passed, failed });
    std.debug.print("Pass rate: {d:.1}%\n\n", .{@as(f64, @floatFromInt(passed)) / @as(f64, @floatFromInt(total)) * 100.0});

    if (failed_files.items.len > 0) {
        std.debug.print("=== Failed Files ===\n", .{});
        for (failed_files.items) |result| {
            std.debug.print("FAIL: {s}\n", .{result.path});
            if (result.error_msg) |msg| {
                std.debug.print("      {s}\n", .{msg});
            }
        }
    }
}

fn collectAndTest(
    allocator: std.mem.Allocator,
    base_path: []const u8,
    iter: *fs.Dir.Iterator,
    total: *usize,
    passed: *usize,
    failed: *usize,
    failed_files: *std.ArrayList(TestResult),
) !void {
    while (try iter.next()) |entry| {
        const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ base_path, entry.name });
        defer allocator.free(full_path);

        if (entry.kind == .directory) {
            std.debug.print(">>> Entering: {s}\n", .{full_path});
            var sub_dir = try fs.cwd().openDir(full_path, .{ .iterate = true });
            defer sub_dir.close();
            var sub_iter = sub_dir.iterate();

            const sub_path = try allocator.dupe(u8, full_path);
            defer allocator.free(sub_path);
            try collectAndTest(allocator, sub_path, &sub_iter, total, passed, failed, failed_files);
            std.debug.print("<<< Done: {s}\n", .{full_path});
        } else if (std.mem.endsWith(u8, entry.name, ".wren")) {
            total.* += 1;
            std.debug.print("    Parsing: {s}...", .{full_path});
            const result = testFile(allocator, full_path);

            if (result.success) {
                passed.* += 1;
                std.debug.print(" ✓\n", .{});
            } else {
                failed.* += 1;
                std.debug.print(" ✗ {s}\n", .{result.error_msg orelse "unknown"});
                const path_copy = allocator.dupe(u8, full_path) catch continue;
                const error_copy = if (result.error_msg) |msg| allocator.dupe(u8, msg) catch null else null;
                failed_files.append(allocator, .{
                    .path = path_copy,
                    .success = false,
                    .error_msg = error_copy,
                }) catch {};
            }
        }
    }
}

fn testFile(allocator: std.mem.Allocator, path: []const u8) TestResult {
    const file = fs.cwd().openFile(path, .{}) catch |err| {
        return .{ .path = path, .success = false, .error_msg = @errorName(err) };
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 4 * 1024 * 1024) catch |err| {
        return .{ .path = path, .success = false, .error_msg = @errorName(err) };
    };
    defer allocator.free(content);

    const expects_error = std.mem.indexOf(u8, content, "// expect error") != null or
        std.mem.indexOf(u8, content, "// expect runtime error") != null;

    var source_file = SourceFile.new(allocator, path, content) catch |err| {
        return .{ .path = path, .success = expects_error, .error_msg = @errorName(err) };
    };
    defer source_file.deinit();

    const lexer = Lexer.new(allocator, source_file) catch |err| {
        return .{ .path = path, .success = expects_error, .error_msg = @errorName(err) };
    };

    var parser = Parser.new(allocator, lexer) catch |err| {
        return .{ .path = path, .success = expects_error, .error_msg = @errorName(err) };
    };
    defer parser.deinit();

    _ = parser.parseModule() catch |err| {
        return .{ .path = path, .success = expects_error, .error_msg = @errorName(err) };
    };

    if (parser.errors.items.len > 0) {
        return .{ .path = path, .success = expects_error, .error_msg = parser.errors.items[0].message };
    }

    return .{ .path = path, .success = true, .error_msg = null };
}
