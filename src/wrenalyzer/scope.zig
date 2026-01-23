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
    inferred_type: ?InferredType = null,

    pub const Kind = enum {
        variable,
        parameter,
        class,
        method,
        field,
        static_field,
        import_var,
    };

    pub const InferredType = enum {
        num,
        string,
        bool_type,
        null_type,
        list,
        map,
        range,
        fn_type,
        fiber,
        unknown,
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

pub const BuiltinMethod = struct {
    name: []const u8,
    signature: []const u8,
};

pub const BUILTIN_METHODS = std.StaticStringMap([]const BuiltinMethod).initComptime(.{
    .{ "System", &[_]BuiltinMethod{
        .{ .name = "clock", .signature = "clock" },
        .{ .name = "gc", .signature = "gc()" },
        .{ .name = "print", .signature = "print()" },
        .{ .name = "print", .signature = "print(object)" },
        .{ .name = "printAll", .signature = "printAll(sequence)" },
        .{ .name = "write", .signature = "write(object)" },
        .{ .name = "writeAll", .signature = "writeAll(sequence)" },
    } },
    .{ "Fiber", &[_]BuiltinMethod{
        .{ .name = "new", .signature = "new(function)" },
        .{ .name = "abort", .signature = "abort(message)" },
        .{ .name = "current", .signature = "current" },
        .{ .name = "suspend", .signature = "suspend()" },
        .{ .name = "yield", .signature = "yield()" },
        .{ .name = "yield", .signature = "yield(value)" },
    } },
    .{ "Num", &[_]BuiltinMethod{
        .{ .name = "fromString", .signature = "fromString(value)" },
        .{ .name = "infinity", .signature = "infinity" },
        .{ .name = "nan", .signature = "nan" },
        .{ .name = "pi", .signature = "pi" },
        .{ .name = "tau", .signature = "tau" },
        .{ .name = "largest", .signature = "largest" },
        .{ .name = "smallest", .signature = "smallest" },
        .{ .name = "maxSafeInteger", .signature = "maxSafeInteger" },
        .{ .name = "minSafeInteger", .signature = "minSafeInteger" },
    } },
    .{ "String", &[_]BuiltinMethod{
        .{ .name = "fromCodePoint", .signature = "fromCodePoint(codePoint)" },
        .{ .name = "fromByte", .signature = "fromByte(byte)" },
    } },
    .{ "List", &[_]BuiltinMethod{
        .{ .name = "new", .signature = "new()" },
        .{ .name = "filled", .signature = "filled(size, element)" },
    } },
    .{ "Map", &[_]BuiltinMethod{
        .{ .name = "new", .signature = "new()" },
    } },
    .{ "Range", &[_]BuiltinMethod{
        .{ .name = "new", .signature = "new(from, to)" },
    } },
});

pub const INSTANCE_METHODS = std.StaticStringMap([]const BuiltinMethod).initComptime(.{
    .{ "num", &[_]BuiltinMethod{
        .{ .name = "abs", .signature = "abs" },
        .{ .name = "acos", .signature = "acos" },
        .{ .name = "asin", .signature = "asin" },
        .{ .name = "atan", .signature = "atan" },
        .{ .name = "atan", .signature = "atan(x)" },
        .{ .name = "cbrt", .signature = "cbrt" },
        .{ .name = "ceil", .signature = "ceil" },
        .{ .name = "cos", .signature = "cos" },
        .{ .name = "floor", .signature = "floor" },
        .{ .name = "fraction", .signature = "fraction" },
        .{ .name = "isInfinity", .signature = "isInfinity" },
        .{ .name = "isInteger", .signature = "isInteger" },
        .{ .name = "isNan", .signature = "isNan" },
        .{ .name = "log", .signature = "log" },
        .{ .name = "log2", .signature = "log2" },
        .{ .name = "exp", .signature = "exp" },
        .{ .name = "min", .signature = "min(other)" },
        .{ .name = "max", .signature = "max(other)" },
        .{ .name = "clamp", .signature = "clamp(min, max)" },
        .{ .name = "pow", .signature = "pow(power)" },
        .{ .name = "round", .signature = "round" },
        .{ .name = "sign", .signature = "sign" },
        .{ .name = "sin", .signature = "sin" },
        .{ .name = "sqrt", .signature = "sqrt" },
        .{ .name = "tan", .signature = "tan" },
        .{ .name = "toString", .signature = "toString" },
        .{ .name = "truncate", .signature = "truncate" },
    } },
    .{ "string", &[_]BuiltinMethod{
        .{ .name = "bytes", .signature = "bytes" },
        .{ .name = "codePoints", .signature = "codePoints" },
        .{ .name = "contains", .signature = "contains(other)" },
        .{ .name = "count", .signature = "count" },
        .{ .name = "endsWith", .signature = "endsWith(suffix)" },
        .{ .name = "indexOf", .signature = "indexOf(search)" },
        .{ .name = "iterate", .signature = "iterate(iterator)" },
        .{ .name = "iteratorValue", .signature = "iteratorValue(iterator)" },
        .{ .name = "replace", .signature = "replace(old, new)" },
        .{ .name = "split", .signature = "split(separator)" },
        .{ .name = "startsWith", .signature = "startsWith(prefix)" },
        .{ .name = "toString", .signature = "toString" },
        .{ .name = "trim", .signature = "trim()" },
        .{ .name = "trimEnd", .signature = "trimEnd()" },
        .{ .name = "trimStart", .signature = "trimStart()" },
    } },
    .{ "list", &[_]BuiltinMethod{
        .{ .name = "add", .signature = "add(item)" },
        .{ .name = "addAll", .signature = "addAll(other)" },
        .{ .name = "clear", .signature = "clear()" },
        .{ .name = "count", .signature = "count" },
        .{ .name = "indexOf", .signature = "indexOf(value)" },
        .{ .name = "insert", .signature = "insert(index, item)" },
        .{ .name = "iterate", .signature = "iterate(iterator)" },
        .{ .name = "iteratorValue", .signature = "iteratorValue(iterator)" },
        .{ .name = "remove", .signature = "remove(value)" },
        .{ .name = "removeAt", .signature = "removeAt(index)" },
        .{ .name = "sort", .signature = "sort()" },
        .{ .name = "sort", .signature = "sort(comparer)" },
        .{ .name = "swap", .signature = "swap(index0, index1)" },
    } },
    .{ "map", &[_]BuiltinMethod{
        .{ .name = "clear", .signature = "clear()" },
        .{ .name = "containsKey", .signature = "containsKey(key)" },
        .{ .name = "count", .signature = "count" },
        .{ .name = "keys", .signature = "keys" },
        .{ .name = "remove", .signature = "remove(key)" },
        .{ .name = "values", .signature = "values" },
    } },
    .{ "bool_type", &[_]BuiltinMethod{
        .{ .name = "toString", .signature = "toString" },
    } },
});

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
    self.declareWithType(name_token, kind, null);
}

pub fn declareWithType(self: *Scope, name_token: Token, kind: Symbol.Kind, inferred_type: ?Symbol.InferredType) void {
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
        .inferred_type = inferred_type,
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
