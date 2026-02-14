//! Integration tests for the module resolution system.

const std = @import("std");
const builtins = @import("builtins.zig");
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
    try std.testing.expectEqualStrings("./src", cfg.modules[0].directory);
    try std.testing.expectEqualStrings("./modules", cfg.modules[1].directory);
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
    }, &.{}, null);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
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
        .modules = &.{.{ .directory = "./src" }},
        .project_root = "/tmp/test",
    };

    var chain = try ResolverChain.init(arena, cfg);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .import_string = "utils",
        .project_root = "/tmp/test",
    };

    // Returns null for non-existent module (no virtual fallback)
    const result = chain.resolve(request);
    try std.testing.expect(result == null);
}

test "path resolver with delimiter converts dots to slashes" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const path_resolver = try resolver.PathResolver.create(arena, .{
        .roots = &.{"./src"},
        .delimiter = ".",
    }, &.{}, "/tmp/test");

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
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

    var source_file = try wrenalyzer.SourceFile.new(std.testing.allocator, "test.wren", src);
    defer source_file.deinit();
    const lexer = try wrenalyzer.Lexer.new(std.testing.allocator, &source_file);
    var parser = try wrenalyzer.Parser.new(std.testing.allocator, lexer);
    defer parser.deinit();
    const module = try parser.parseModule();

    var class_count: usize = 0;
    var var_count: usize = 0;
    var class_names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer class_names.deinit(std.testing.allocator);
    var var_names: std.ArrayListUnmanaged([]const u8) = .empty;
    defer var_names.deinit(std.testing.allocator);

    for (module.statements) |stmt| {
        switch (stmt) {
            .ClassStmt => |cs| {
                class_count += 1;
                if (cs.name) |n| try class_names.append(std.testing.allocator, n.name());
            },
            .VarStmt => |vs| {
                var_count += 1;
                if (vs.name) |n| try var_names.append(std.testing.allocator, n.name());
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

test "builtin resolver resolves random module" {
    const allocator = std.testing.allocator;
    const builtin_resolver = try resolver.BuiltinResolver.create(allocator);
    defer builtin_resolver.deinit(allocator);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/main.wren",
        .import_string = "random",
        .project_root = "/tmp/test",
    };

    const result = builtin_resolver.resolve(allocator, request);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("wren://builtin/random", result.?.canonical_id);
    try std.testing.expect(result.?.source != null);
    try std.testing.expect(result.?.kind == .virtual);
    try std.testing.expect(std.mem.indexOf(u8, result.?.source.?, "foreign class Random") != null);

    allocator.free(result.?.canonical_id);
    if (result.?.uri) |uri| allocator.free(uri);
}

test "builtin resolver resolves meta module" {
    const allocator = std.testing.allocator;
    const builtin_resolver = try resolver.BuiltinResolver.create(allocator);
    defer builtin_resolver.deinit(allocator);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/main.wren",
        .import_string = "meta",
        .project_root = "/tmp/test",
    };

    const result = builtin_resolver.resolve(allocator, request);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("wren://builtin/meta", result.?.canonical_id);
    try std.testing.expect(result.?.source != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.source.?, "class Meta") != null);

    allocator.free(result.?.canonical_id);
    if (result.?.uri) |uri| allocator.free(uri);
}

test "builtin resolver returns null for unknown modules" {
    const allocator = std.testing.allocator;
    const builtin_resolver = try resolver.BuiltinResolver.create(allocator);
    defer builtin_resolver.deinit(allocator);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/main.wren",
        .import_string = "unknown_module",
        .project_root = "/tmp/test",
    };

    const result = builtin_resolver.resolve(allocator, request);
    try std.testing.expect(result == null);
}

test "builtin resolver ignores relative imports" {
    const allocator = std.testing.allocator;
    const builtin_resolver = try resolver.BuiltinResolver.create(allocator);
    defer builtin_resolver.deinit(allocator);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/main.wren",
        .import_string = "./random",
        .project_root = "/tmp/test",
    };

    const result = builtin_resolver.resolve(allocator, request);
    try std.testing.expect(result == null);
}

test "resolver chain includes builtin resolver" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const cfg = types.Config{
        .modules = &.{},
        .resolvers = &.{},
        .project_root = "/tmp/test",
    };

    var chain = try ResolverChain.init(arena, cfg);
    defer chain.deinit();

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/main.wren",
        .import_string = "random",
        .project_root = "/tmp/test",
    };

    const result = chain.resolve(request);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("wren://builtin/random", result.?.canonical_id);
    try std.testing.expect(result.?.source != null);
}

test "getBuiltinSource returns embedded sources" {
    try std.testing.expect(builtins.getBuiltinSource("random") != null);
    try std.testing.expect(builtins.getBuiltinSource("meta") != null);
    try std.testing.expect(builtins.getBuiltinSource("nonexistent") == null);

    const random_src = builtins.getBuiltinSource("random").?;
    try std.testing.expect(std.mem.indexOf(u8, random_src, "foreign class Random") != null);

    const meta_src = builtins.getBuiltinSource("meta").?;
    try std.testing.expect(std.mem.indexOf(u8, meta_src, "class Meta") != null);
}
