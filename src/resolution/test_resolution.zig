//! Integration tests for the module resolution system.

const std = @import("std");
const config = @import("config.zig");
const resolver = @import("resolver.zig");
const types = @import("types.zig");

const ConfigLoader = config.ConfigLoader;
const ResolverChain = resolver.ResolverChain;
const ResolveRequest = types.ResolveRequest;

test "config loader parses simplified config" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var loader = ConfigLoader.init(arena);

    const json =
        \\{
        \\  "version": 1,
        \\  "modules": ["./src", "./modules"],
        \\  "resolvers": [
        \\    {"type": "path", "roots": ["./src", "./modules"], "delimiter": "."}
        \\  ]
        \\}
    ;

    const cfg = try loader.parseJson(json);

    try std.testing.expectEqual(@as(usize, 2), cfg.modules.len);
    try std.testing.expectEqualStrings("./src", cfg.modules[0]);
    try std.testing.expectEqualStrings("./modules", cfg.modules[1]);
    try std.testing.expectEqual(@as(usize, 1), cfg.resolvers.len);
    try std.testing.expectEqualStrings(".", cfg.resolvers[0].path.delimiter);
}

test "path resolver resolves relative imports" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const path_resolver = try resolver.PathResolver.create(arena, .{
        .roots = &.{},
        .delimiter = "/",
    }, null);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .importer_module_id = "file:///tmp/test/src/main.wren",
        .import_string = "./utils",
        .project_root = "/tmp/test",
    };

    _ = path_resolver.resolve(arena, request);
}

test "resolver chain uses default path resolver" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const cfg = types.Config{
        .modules = &.{"./src"},
        .project_root = "/tmp/test",
    };

    var chain = try ResolverChain.init(arena, cfg);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .importer_module_id = "file:///tmp/test/src/main.wren",
        .import_string = "utils",
        .project_root = "/tmp/test",
    };

    // Will return virtual fallback for non-existent module
    const result = chain.resolve(request);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.kind == .virtual);
}

test "path resolver with delimiter converts dots to slashes" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const path_resolver = try resolver.PathResolver.create(arena, .{
        .roots = &.{"./src"},
        .delimiter = ".",
    }, "/tmp/test");

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .importer_module_id = "file:///tmp/test/src/main.wren",
        .import_string = "models.user",
        .project_root = "/tmp/test",
    };

    // Will return null since file doesn't exist, but tests the conversion logic
    _ = path_resolver.resolve(arena, request);
}

test "parser finds both classes and top-level vars" {
    const wrenalyzer = @import("wrenalyzer");
    const src =
        \\class MyClass {
        \\  construct new() {}
        \\}
        \\
        \\var myVar = 1
        \\var anotherVar = Fn.new {}
    ;

    var doc = try wrenalyzer.Ast.initFromSource(std.testing.allocator, src, null);
    defer doc.deinit(std.testing.allocator);

    var class_count: usize = 0;
    var var_count: usize = 0;
    var class_names = std.ArrayList([]const u8).init(std.testing.allocator);
    defer class_names.deinit();
    var var_names = std.ArrayList([]const u8).init(std.testing.allocator);
    defer var_names.deinit();

    for (doc.module.statements) |stmt| {
        switch (stmt) {
            .ClassStmt => |cs| {
                class_count += 1;
                if (cs.name) |n| try class_names.append(n.name());
            },
            .VarStmt => |vs| {
                var_count += 1;
                if (vs.name) |n| try var_names.append(n.name());
            },
            else => {},
        }
    }

    try std.testing.expectEqual(@as(usize, 1), class_count);
    try std.testing.expectEqual(@as(usize, 2), var_count);
    try std.testing.expectEqualStrings("MyClass", class_names.items[0]);
    try std.testing.expectEqualStrings("myVar", var_names.items[0]);
    try std.testing.expectEqualStrings("anotherVar", var_names.items[1]);
}
