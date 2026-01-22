//! Scope - manages lexical scopes for variable resolution.
//!
//! Tracks variable declarations and resolves references during semantic analysis.

const std = @import("std");
const Token = @import("token.zig").Token;
const Reporter = @import("reporter.zig");

pub const Scope = @This();

pub const Symbol = struct {
    name: []const u8,
    token: Token,
    kind: Kind,

    pub const Kind = enum {
        variable,
        parameter,
        class,
        method,
        field,
        static_field,
        import_var,
    };
};

allocator: std.mem.Allocator,
reporter: *Reporter,
scopes: std.ArrayListUnmanaged(?std.StringHashMapUnmanaged(Symbol)),
forward_references: std.ArrayListUnmanaged(Token),

const BUILTINS = [_][]const u8{
    "Bool",    "Class",   "Fiber",  "Fn",    "List",
    "Map",     "Null",    "Num",    "Object", "Range",
    "Sequence", "String", "System",
};

pub fn init(allocator: std.mem.Allocator, reporter: *Reporter) !Scope {
    var self = Scope{
        .allocator = allocator,
        .reporter = reporter,
        .scopes = .empty,
        .forward_references = .empty,
    };

    var module_scope: std.StringHashMapUnmanaged(Symbol) = .empty;
    for (BUILTINS) |name| {
        try module_scope.put(allocator, name, .{
            .name = name,
            .token = undefined,
            .kind = .class,
        });
    }
    try self.scopes.append(allocator, module_scope);

    return self;
}

pub fn deinit(self: *Scope) void {
    for (self.scopes.items) |*maybe_scope| {
        if (maybe_scope.*) |*scope| {
            scope.deinit(self.allocator);
        }
    }
    self.scopes.deinit(self.allocator);
    self.forward_references.deinit(self.allocator);
}

pub fn declare(self: *Scope, name_token: Token, kind: Symbol.Kind) void {
    const name = name_token.name();

    var scope = &(self.scopes.items[self.scopes.items.len - 1] orelse {
        self.reporter.reportError(name_token, "Cannot declare inside class boundary marker");
        return;
    });

    if (scope.get(name)) |existing| {
        _ = existing;
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Variable '{s}' is already defined in this scope", .{name}) catch "Variable already defined";
        self.reporter.reportError(name_token, msg);
        return;
    }

    scope.put(self.allocator, name, .{
        .name = name,
        .token = name_token,
        .kind = kind,
    }) catch {
        self.reporter.reportError(name_token, "Failed to declare variable");
    };
}

pub fn resolve(self: *Scope, name_token: Token) ?Symbol {
    const name = name_token.name();
    var reached_class = false;

    var i: usize = self.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const maybe_scope = self.scopes.items[i];
        if (maybe_scope == null) {
            reached_class = true;
            continue;
        }

        if (maybe_scope.?.get(name)) |sym| {
            return sym;
        }
    }

    if (reached_class) {
        if (name.len > 0 and std.ascii.isLower(name[0])) {
            return null;
        } else {
            if (self.scopes.items[0]) |module_scope| {
                if (module_scope.get(name)) |sym| {
                    return sym;
                }
            }
            self.forward_references.append(self.allocator, name_token) catch {};
            return null;
        }
    }

    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "Variable '{s}' is not defined", .{name}) catch "Variable not defined";
    self.reporter.reportError(name_token, msg);
    return null;
}

pub fn begin(self: *Scope) void {
    self.scopes.append(self.allocator, .empty) catch {};
}

pub fn end(self: *Scope) void {
    if (self.scopes.items.len > 1) {
        if (self.scopes.pop()) |maybe_scope| {
            if (maybe_scope) |scope| {
                var s = scope;
                s.deinit(self.allocator);
            }
        }
    }
}

pub fn beginClass(self: *Scope) void {
    self.scopes.append(self.allocator, null) catch {};
}

pub fn endClass(self: *Scope) void {
    if (self.scopes.items.len > 0) {
        _ = self.scopes.pop();
    }
}

pub fn checkForwardReferences(self: *Scope) void {
    const module_scope = self.scopes.items[0] orelse return;

    for (self.forward_references.items) |ref| {
        if (module_scope.get(ref.name()) == null) {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "Variable '{s}' is not defined", .{ref.name()}) catch "Variable not defined";
            self.reporter.reportError(ref, msg);
        }
    }
}

pub fn currentDepth(self: *const Scope) usize {
    return self.scopes.items.len;
}
