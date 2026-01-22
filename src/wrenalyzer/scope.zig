const std = @import("std");
const Scope = @This();
const token = @import("token.zig");
const Token = token.Token;
const ast = @import("ast.zig");
const Node = ast.Node;

scope: std.StringHashMapUnmanaged(Token),

pub fn new() Scope {
    return .{
        .scope = .{},
    };
}

pub fn declare(self: *Scope, name: Node) void {
    const tok = name.identifierToken();

    if (self.scope.get(tok.name())) |val| {
        _ = .{val};
        std.debug.print("Already declared {s}\n", .{tok.name()});
        return;
    }

    self.scope.put(tok.name(), name) catch @panic("Error adding to scope");
}
