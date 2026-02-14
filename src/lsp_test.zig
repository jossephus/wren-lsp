const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;
const testing = @import("testing.zig");
const TestContext = testing.TestContext;

const test_uri = "file:///test/main.wren";

fn rangeEq(a: types.Range, b: types.Range) bool {
    return a.start.line == b.start.line and
        a.start.character == b.start.character and
        a.end.line == b.end.line and
        a.end.character == b.end.character;
}

fn containsLocation(locations: []const types.Location, uri: []const u8, range: types.Range) bool {
    for (locations) |loc| {
        if (std.mem.eql(u8, loc.uri, uri) and rangeEq(loc.range, range)) return true;
    }
    return false;
}

fn containsHighlight(highlights: []const types.DocumentHighlight, range: types.Range, kind: types.DocumentHighlightKind) bool {
    for (highlights) |h| {
        if (rangeEq(h.range, range) and h.kind == kind) return true;
    }
    return false;
}

fn hoverMarkdown(hover: types.Hover) ?[]const u8 {
    return switch (hover.contents) {
        .MarkupContent => |mc| mc.value,
        else => null,
    };
}

fn hasUriRangeFields(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct" => @hasField(T, "uri") and @hasField(T, "range"),
        else => false,
    };
}

fn collectLocations(arena: std.mem.Allocator, value: anytype) ![]const types.Location {
    var out: std.ArrayListUnmanaged(types.Location) = .empty;
    try appendLocations(arena, &out, value);
    return out.items;
}

fn appendLocations(arena: std.mem.Allocator, out: *std.ArrayListUnmanaged(types.Location), value: anytype) !void {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .optional => {
            if (value) |v| try appendLocations(arena, out, v);
        },
        .pointer => |ptr| {
            if (ptr.size == .slice) {
                if (comptime hasUriRangeFields(ptr.child)) {
                    for (value) |item| try out.append(arena, item);
                } else {
                    for (value) |item| try appendLocations(arena, out, item);
                }
            }
        },
        .array => {
            if (comptime hasUriRangeFields(std.meta.Child(T))) {
                for (value) |item| try out.append(arena, item);
            } else {
                for (value) |item| try appendLocations(arena, out, item);
            }
        },
        .@"struct" => {
            if (comptime hasUriRangeFields(T)) {
                try out.append(arena, value);
            } else {
                inline for (std.meta.fields(T)) |field| {
                    try appendLocations(arena, out, @field(value, field.name));
                }
            }
        },
        .@"union" => {
            switch (value) {
                inline else => |payload| try appendLocations(arena, out, payload),
            }
        },
        else => {},
    }
}

// ============== Completion ==============

test "completion: class names" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\class Foo {}
        \\class Bar {}
        \\var x = F
    );

    const result = try ctx.completion(2, 9);
    try std.testing.expect(TestContext.hasCompletionLabel(result, "Foo"));
    try std.testing.expect(TestContext.hasCompletionLabel(result, "Bar"));
}

test "completion: variables in scope" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\var myVar = 42
        \\var other = m
    );

    const result = try ctx.completion(1, 13);
    try std.testing.expect(TestContext.hasCompletionLabel(result, "myVar"));
}

// ============== Hover ==============

test "hover: class name" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\class MyClass {}
        \\var x = MyClass
    );

    const result = try ctx.hover(1, 10);
    const hover = result orelse return error.TestUnexpectedResult;
    const markdown = hoverMarkdown(hover) orelse return error.TestUnexpectedResult;
    try std.testing.expect(std.mem.containsAtLeast(u8, markdown, 1, "**class**"));
    try std.testing.expect(std.mem.containsAtLeast(u8, markdown, 1, "`MyClass`"));
    try std.testing.expect(rangeEq(hover.range.?, .{
        .start = .{ .line = 1, .character = 8 },
        .end = .{ .line = 1, .character = 15 },
    }));
}

test "hover: unknown position returns null" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\// just a comment
    );

    const result = try ctx.hover(0, 0);
    try std.testing.expect(result == null);
}

// ============== Document Symbols ==============

test "documentSymbol: class and var" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\class Animal {
        \\  construct new() {}
        \\  speak() { return "..." }
        \\}
        \\var count = 0
    );

    const result = try ctx.documentSymbols();
    const symbols_union = result orelse return error.TestUnexpectedResult;
    const symbols = switch (symbols_union) {
        .array_of_DocumentSymbol => |s| s,
        else => return error.TestUnexpectedResult,
    };

    try std.testing.expectEqual(@as(usize, 2), symbols.len);
    try std.testing.expectEqualStrings("Animal", symbols[0].name);
    try std.testing.expectEqual(types.SymbolKind.Class, symbols[0].kind);
    try std.testing.expect(symbols[0].children != null);
    try std.testing.expectEqual(@as(usize, 2), symbols[0].children.?.len);
    try std.testing.expectEqualStrings("new", symbols[0].children.?[0].name);
    try std.testing.expectEqualStrings("speak", symbols[0].children.?[1].name);
    try std.testing.expectEqualStrings("count", symbols[1].name);
    try std.testing.expectEqual(types.SymbolKind.Variable, symbols[1].kind);
}

// ============== Semantic Tokens ==============

test "semanticTokens: produces tokens" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\class Foo {
        \\  construct new() {}
        \\}
        \\var x = 42
    );

    const result = try ctx.semanticTokens();
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.data.len > 0);
    try std.testing.expect(result.?.data.len % 5 == 0);
}

// ============== Definition ==============

test "definition: go to variable declaration" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\var foo = 1
        \\var bar = foo
    );

    const result = try ctx.definition(1, 11);
    const locations = try collectLocations(ctx.arena.allocator(), result);
    try std.testing.expect(locations.len > 0);
    try std.testing.expect(containsLocation(locations, test_uri, .{
        .start = .{ .line = 0, .character = 4 },
        .end = .{ .line = 0, .character = 7 },
    }));
}

// ============== References ==============

test "references: find variable usages" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\var foo = 1
        \\var bar = foo
        \\var baz = foo
    );

    const result = try ctx.references(0, 5);
    const locations = result orelse return error.TestUnexpectedResult;
    try std.testing.expect(locations.len >= 3);
    try std.testing.expect(containsLocation(locations, test_uri, .{
        .start = .{ .line = 0, .character = 4 },
        .end = .{ .line = 0, .character = 7 },
    }));
    try std.testing.expect(containsLocation(locations, test_uri, .{
        .start = .{ .line = 1, .character = 10 },
        .end = .{ .line = 1, .character = 13 },
    }));
    try std.testing.expect(containsLocation(locations, test_uri, .{
        .start = .{ .line = 2, .character = 10 },
        .end = .{ .line = 2, .character = 13 },
    }));
}

// ============== Rename ==============

test "rename: no crash on valid position" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\var foo = 1
        \\var bar = foo
    );

    const result = try ctx.rename(0, 5, "newName");
    try std.testing.expect(result != null);
    const changes = result.?.changes orelse return error.TestUnexpectedResult;
    const edits_for_file = changes.map.get(test_uri);
    try std.testing.expect(edits_for_file != null);
    try std.testing.expect(edits_for_file.?.len >= 2);
    var saw_decl = false;
    var saw_use = false;
    for (edits_for_file.?) |edit| {
        if (std.mem.eql(u8, edit.newText, "newName") and
            rangeEq(edit.range, .{
                .start = .{ .line = 0, .character = 4 },
                .end = .{ .line = 0, .character = 7 },
            }))
        {
            saw_decl = true;
        }
        if (std.mem.eql(u8, edit.newText, "newName") and
            rangeEq(edit.range, .{
                .start = .{ .line = 1, .character = 10 },
                .end = .{ .line = 1, .character = 13 },
            }))
        {
            saw_use = true;
        }
    }
    try std.testing.expect(saw_decl);
    try std.testing.expect(saw_use);
}

// ============== Folding Ranges ==============

test "foldingRange: class body" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\class Foo {
        \\  construct new() {}
        \\  method() {
        \\    return 1
        \\  }
        \\}
    );

    const result = try ctx.foldingRanges();
    const ranges = result orelse return error.TestUnexpectedResult;
    try std.testing.expect(ranges.len >= 2);

    var has_class_range = false;
    var has_method_range = false;
    for (ranges) |r| {
        if (r.startLine == 0 and r.endLine == 5) has_class_range = true;
        if (r.startLine == 2 and r.endLine == 4) has_method_range = true;
    }
    try std.testing.expect(has_class_range);
    try std.testing.expect(has_method_range);
}

// ============== Document Highlights ==============

test "documentHighlight: variable" {
    var ctx = TestContext.init();
    defer ctx.deinit();

    try ctx.openDocument(
        \\var foo = 1
        \\var bar = foo
    );

    const result = try ctx.documentHighlights(0, 5);
    const highlights = result orelse return error.TestUnexpectedResult;
    try std.testing.expect(highlights.len >= 2);
    try std.testing.expect(containsHighlight(highlights, .{
        .start = .{ .line = 0, .character = 4 },
        .end = .{ .line = 0, .character = 7 },
    }, .Write));
    try std.testing.expect(containsHighlight(highlights, .{
        .start = .{ .line = 1, .character = 10 },
        .end = .{ .line = 1, .character = 13 },
    }, .Read));
}
