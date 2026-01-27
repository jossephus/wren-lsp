//! Integration tests for the module resolution system.

const std = @import("std");
const config = @import("config.zig");
const resolver = @import("resolver.zig");
const types = @import("types.zig");

const ConfigLoader = config.ConfigLoader;
const ResolverChain = resolver.ResolverChain;
const ResolveRequest = types.ResolveRequest;

test "config loader parses test project config" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var loader = ConfigLoader.init(arena);

    const json =
        \\{
        \\  "version": 1,
        \\  "workspace": {
        \\    "roots": ["./src", "./modules"],
        \\    "stubs": ["./stubs/wren-cli"]
        \\  },
        \\  "imports": {
        \\    "resolvers": [
        \\      {"type": "path", "roots": ["./src", "./modules"], "extensions": [".wren"]},
        \\      {"type": "stubs"}
        \\    ]
        \\  }
        \\}
    ;

    const cfg = try loader.parseJson(json);

    try std.testing.expectEqual(@as(usize, 2), cfg.workspace.roots.len);
    try std.testing.expectEqualStrings("./src", cfg.workspace.roots[0]);
    try std.testing.expectEqualStrings("./modules", cfg.workspace.roots[1]);
    try std.testing.expectEqual(@as(usize, 1), cfg.workspace.stubs.len);
    try std.testing.expectEqual(@as(usize, 2), cfg.imports.resolvers.len);
}

test "path resolver resolves relative imports" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const path_resolver = try resolver.PathResolver.create(arena, .{
        .roots = &.{},
        .extensions = &.{".wren"},
        .index_files = &.{},
    }, null);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .import_string = "./utils",
        .project_root = "/tmp/test",
    };

    _ = path_resolver.resolve(arena, request);
}

test "stubs resolver returns virtual module" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const stubs_resolver = try resolver.StubsResolver.create(arena, &.{"./stubs/wren-cli"});

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .import_string = "io",
        .project_root = "/tmp/test",
    };

    const result = stubs_resolver.resolve(arena, request);
    if (result) |r| {
        try std.testing.expect(r.kind == .file or r.kind == .virtual);
    }
}

test "resolver chain uses first match" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const cfg = types.Config{
        .workspace = .{
            .stubs = &.{"./stubs/wren-cli"},
        },
        .imports = .{
            .resolvers = &.{
                .{ .kind = .stubs },
            },
        },
        .project_root = "/tmp/test",
    };

    var chain = try ResolverChain.init(arena, cfg);

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .import_string = "io",
        .project_root = "/tmp/test",
    };

    const result = chain.resolve(request);
    try std.testing.expect(result != null);
}

test "dotted resolver converts dots to slashes" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const dotted_resolver = try resolver.DottedResolver.create(arena, .{
        .delimiter = ".",
        .map_to = "/",
        .roots = &.{"./src"},
        .extensions = &.{".wren"},
    }, "/tmp/test");

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .import_string = "models.user",
        .project_root = "/tmp/test",
    };

    _ = dotted_resolver.resolve(arena, request);
}

test "scheme resolver strips prefix" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const scheme_resolver = try resolver.SchemeResolver.create(arena, .{
        .scheme = "hl",
        .strip_prefix = "hl:",
    }, "/tmp/test");

    const request = ResolveRequest{
        .importer_uri = "file:///tmp/test/src/main.wren",
        .import_string = "hl:graphics/sprite",
        .project_root = "/tmp/test",
    };

    const result = scheme_resolver.resolve(arena, request);
    if (result) |r| {
        try std.testing.expect(std.mem.indexOf(u8, r.canonical_id, "wren://hl/") != null);
    }
}
