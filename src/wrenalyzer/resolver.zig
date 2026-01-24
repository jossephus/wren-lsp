//! Resolver - walks the AST and resolves identifiers to their declarations.
//!
//! Builds a symbol table and reports semantic errors like undefined variables.

const std = @import("std");
const ast = @import("ast.zig");
const visitor = @import("visitor.zig");
const Scope = @import("scope.zig");
const Token = @import("token.zig").Token;
const Reporter = @import("reporter.zig");

pub const Resolver = @This();

allocator: std.mem.Allocator,
scope: Scope,
reporter: *Reporter,
class_depth: usize,
module: ?*ast.Module,

pub fn init(allocator: std.mem.Allocator, reporter: *Reporter) !Resolver {
    return .{
        .allocator = allocator,
        .scope = try Scope.init(allocator, reporter),
        .reporter = reporter,
        .class_depth = 0,
        .module = null,
    };
}

pub fn deinit(self: *Resolver) void {
    self.scope.deinit();
}

pub fn resolve(self: *Resolver, module: *ast.Module) void {
    self.module = module;
    for (module.statements) |*stmt| {
        self.resolveNode(stmt);
    }
    self.scope.checkForwardReferences();
}

fn resolveNode(self: *Resolver, node: *const ast.Node) void {
    switch (node.*) {
        .ClassStmt => |stmt| self.visitClassStmt(stmt),
        .VarStmt => |stmt| self.visitVarStmt(stmt),
        .ImportStmt => |stmt| self.visitImportStmt(stmt),
        .ForStmt => |stmt| self.visitForStmt(stmt),
        .BlockStmt => |stmt| self.visitBlockStmt(stmt),
        .IfStmt => |stmt| self.visitIfStmt(stmt),
        .WhileStmt => |stmt| self.visitWhileStmt(stmt),
        .ReturnStmt => |stmt| self.visitReturnStmt(stmt),
        .BreakStmt, .ContinueStmt => {},
        .Body => |body| self.visitBody(body),
        .Method => |method| self.visitMethod(method),
        .CallExpr => |expr| self.visitCallExpr(expr),
        .AssignmentExpr => |expr| self.visitAssignmentExpr(expr),
        .InfixExpr => |expr| self.visitInfixExpr(expr),
        .PrefixExpr => |expr| self.visitPrefixExpr(expr),
        .GroupingExpr => |expr| self.visitGroupingExpr(expr),
        .ListExpr => |expr| self.visitListExpr(expr),
        .MapExpr => |expr| self.visitMapExpr(expr),
        .SubscriptExpr => |expr| self.visitSubscriptExpr(expr),
        .SuperExpr => |expr| self.visitSuperExpr(expr),
        .Module => {},
        .NumExpr, .BoolExpr, .NullExpr, .StringExpr, .ThisExpr, .FieldExpr, .StaticFieldExpr => {},
    }
}

fn visitClassStmt(self: *Resolver, stmt: ast.ClassStmt) void {
    if (stmt.name) |name| {
        self.scope.declare(name, .class);
    }

    self.scope.beginClass();
    self.class_depth += 1;

    for (stmt.methods) |*method| {
        self.resolveNode(method);
    }

    self.class_depth -= 1;
    self.scope.endClass();
}

fn visitMethod(self: *Resolver, method: ast.Method) void {
    if (method.body.*) |*body| {
        self.resolveNode(body);
    }
}

fn visitBody(self: *Resolver, body: ast.Body) void {
    self.scope.begin();

    for (body.parameters) |param| {
        self.scope.declare(param, .parameter);
    }

    if (body.expression) |expr| {
        self.resolveNode(expr);
    }

    if (body.statements) |stmts| {
        for (stmts) |*stmt| {
            self.resolveNode(stmt);
        }
    }

    self.scope.end();
}

fn visitVarStmt(self: *Resolver, stmt: ast.VarStmt) void {
    var inferred_type: ?Scope.Symbol.InferredType = null;
    var fn_arity: ?usize = null;
    var class_name: ?[]const u8 = null;

    if (stmt.initializer.*) |*initializer| {
        self.resolveNode(initializer);
        inferred_type = self.inferType(initializer);
        if (inferred_type == .fn_type) {
            fn_arity = self.inferFnArity(initializer);
        } else if (inferred_type == .class_type) {
            class_name = self.inferClassName(initializer);
        }
    }

    if (stmt.name) |name| {
        self.scope.declareWithType(name, .variable, inferred_type, fn_arity, class_name);
    }
}

fn inferType(self: *Resolver, node: *const ast.Node) ?Scope.Symbol.InferredType {
    return switch (node.*) {
        .CallExpr => |expr| blk: {
            if (self.isFnNewCall(expr)) break :blk .fn_type;
            if (self.classNewName(expr) != null) break :blk .class_type;
            break :blk null;
        },
        .NumExpr => .num,
        .StringExpr => .string,
        .BoolExpr => .bool_type,
        .NullExpr => .null_type,
        .ListExpr => .list,
        .MapExpr => .map,
        else => null,
    };
}

fn inferClassName(self: *Resolver, node: *const ast.Node) ?[]const u8 {
    return switch (node.*) {
        .CallExpr => |expr| self.classNewName(expr),
        else => null,
    };
}

fn inferFnArity(self: *Resolver, node: *const ast.Node) ?usize {
    return switch (node.*) {
        .CallExpr => |expr| blk: {
            if (!self.isFnNewCall(expr)) break :blk null;
            if (expr.blockArgument.*) |*block| {
                return switch (block.*) {
                    .Body => |body| body.parameters.len,
                    else => null,
                };
            }
            break :blk null;
        },
        else => null,
    };
}

fn isFnNewCall(self: *Resolver, expr: ast.CallExpr) bool {
    _ = self;
    if (!std.mem.eql(u8, expr.name.name(), "new")) return false;
    const receiver = expr.receiver orelse return false;
    switch (receiver.*) {
        .CallExpr => |recv_call| {
            if (recv_call.receiver != null) return false;
            return std.mem.eql(u8, recv_call.name.name(), "Fn");
        },
        else => return false,
    }
}

fn classNewName(self: *Resolver, expr: ast.CallExpr) ?[]const u8 {
    _ = self;
    if (!std.mem.eql(u8, expr.name.name(), "new")) return null;
    const receiver = expr.receiver orelse return null;
    switch (receiver.*) {
        .CallExpr => |recv_call| {
            if (recv_call.receiver != null) return null;
            return recv_call.name.name();
        },
        else => return null,
    }
}

fn visitImportStmt(self: *Resolver, stmt: ast.ImportStmt) void {
    if (stmt.variables) |vars| {
        for (vars) |maybe_var| {
            if (maybe_var) |v| {
                self.scope.declare(v, .import_var);
            }
        }
    }
}

fn visitForStmt(self: *Resolver, stmt: ast.ForStmt) void {
    self.scope.begin();

    if (stmt.variable) |v| {
        self.scope.declare(v, .variable);
    }

    self.resolveNode(stmt.iterator);
    self.resolveNode(stmt.body);

    self.scope.end();
}

fn visitBlockStmt(self: *Resolver, stmt: ast.BlockStmt) void {
    self.scope.begin();

    for (stmt.statements) |*s| {
        self.resolveNode(s);
    }

    self.scope.end();
}

fn visitIfStmt(self: *Resolver, stmt: ast.IfStmt) void {
    self.resolveNode(stmt.condition);
    self.resolveNode(stmt.thenBranch);
    if (stmt.elseBranch) |else_branch| {
        self.resolveNode(else_branch);
    }
}

fn visitWhileStmt(self: *Resolver, stmt: ast.WhileStmt) void {
    self.resolveNode(stmt.condition);
    self.resolveNode(stmt.body);
}

fn visitReturnStmt(self: *Resolver, stmt: ast.ReturnStmt) void {
    if (stmt.value.*) |*val| {
        self.resolveNode(val);
    }
}

fn visitCallExpr(self: *Resolver, expr: ast.CallExpr) void {
    if (expr.receiver) |recv| {
        self.resolveNode(recv);
        self.checkFnCallArity(recv, expr);
        self.checkBuiltinCallArity(recv, expr);
        self.checkUserMethodArity(recv, expr);
    } else {
        if (self.scope.resolveOptional(expr.name) == null and self.class_depth == 0) {
            _ = self.scope.resolve(expr.name);
        }
    }

    for (expr.arguments) |*arg| {
        self.resolveNode(@constCast(arg));
    }

    if (expr.blockArgument.*) |*block| {
        self.resolveNode(block);
    }
}

fn checkFnCallArity(self: *Resolver, receiver: *const ast.Node, expr: ast.CallExpr) void {
    if (!std.mem.eql(u8, expr.name.name(), "call")) return;

    const recv_call = switch (receiver.*) {
        .CallExpr => |call| call,
        else => return,
    };

    if (recv_call.receiver != null) return;

    const sym = self.scope.resolveOptional(recv_call.name) orelse return;
    if (sym.inferred_type != .fn_type) return;
    const arity = sym.fn_arity orelse return;

    if (expr.arguments.len == arity) return;

    var buf: [128]u8 = undefined;
    const msg = std.fmt.bufPrint(
        &buf,
        "Expected {d} arguments for call, found {d}",
        .{ arity, expr.arguments.len },
    ) catch "Incorrect argument count";
    self.reporter.reportError(expr.name, msg);
}

fn checkBuiltinCallArity(self: *Resolver, receiver: *const ast.Node, expr: ast.CallExpr) void {
    const inferred_type = self.receiverInferredType(receiver) orelse return;
    const type_name = @tagName(inferred_type);
    const methods = Scope.INSTANCE_METHODS.get(type_name) orelse return;

    const call_name = expr.name.name();
    var arg_count: usize = expr.arguments.len;
    if (expr.blockArgument.* != null) {
        arg_count += 1;
    }

    var arities: [8]usize = undefined;
    var arity_len: usize = 0;
    var matches_arity = false;
    for (methods) |method| {
        if (!std.mem.eql(u8, method.name, call_name)) continue;
        const arity = builtinMethodArity(method.signature);
        if (!containsArity(arities[0..arity_len], arity)) {
            if (arity_len < arities.len) {
                arities[arity_len] = arity;
                arity_len += 1;
            }
        }
        if (arity == arg_count) {
            matches_arity = true;
            break;
        }
    }

    if (arity_len == 0 or matches_arity) return;

    var list_buf: [64]u8 = undefined;
    var list_stream = std.io.fixedBufferStream(&list_buf);
    const list_writer = list_stream.writer();
    for (arities[0..arity_len], 0..) |arity, i| {
        if (i > 0) list_writer.writeAll(" or ") catch @panic("Error formatting arity list");
        list_writer.print("{d}", .{arity}) catch @panic("Error formatting arity list");
    }
    const list_slice = list_stream.getWritten();

    var buf: [160]u8 = undefined;
    const msg = std.fmt.bufPrint(
        &buf,
        "Expected {s} arguments for {s}.{s}, found {d}",
        .{ list_slice, type_name, call_name, arg_count },
    ) catch "Incorrect argument count";
    self.reporter.reportError(expr.name, msg);
}

fn checkUserMethodArity(self: *Resolver, receiver: *const ast.Node, expr: ast.CallExpr) void {
    const class_info = self.resolveClassReceiver(receiver) orelse return;
    const module = self.module orelse return;

    const class_stmt = self.findClassStmt(module, class_info.name) orelse return;

    const call_name = expr.name.name();
    var arg_count: usize = expr.arguments.len;
    if (expr.blockArgument.* != null) {
        arg_count += 1;
    }

    var arities: [8]usize = undefined;
    var arity_len: usize = 0;
    var matches_arity = false;

    for (class_stmt.methods) |method_node| {
        const method = switch (method_node) {
            .Method => |value| value,
            else => continue,
        };

        if (method.name == null) continue;
        if (!std.mem.eql(u8, method.name.?.name(), call_name)) continue;

        if (class_info.is_static) {
            if (method.staticKeyword == null and method.constructKeyword == null) continue;
        } else {
            if (method.staticKeyword != null) continue;
            if (method.constructKeyword != null) continue;
        }

        const arity = method.parameters.len;
        if (!containsArity(arities[0..arity_len], arity)) {
            if (arity_len < arities.len) {
                arities[arity_len] = arity;
                arity_len += 1;
            }
        }

        if (arity == arg_count) {
            matches_arity = true;
            break;
        }
    }

    if (arity_len == 0 or matches_arity) return;

    const kind_label = if (class_info.is_static) "static" else "instance";
    const arity_list = self.formatArityList(arities[0..arity_len]);
    var buf: [192]u8 = undefined;
    const msg = std.fmt.bufPrint(
        &buf,
        "Expected {s} arguments for {s} {s}.{s}, found {d}",
        .{ arity_list, kind_label, class_info.name, call_name, arg_count },
    ) catch "Incorrect argument count";
    self.reporter.reportError(expr.name, msg);
}

fn resolveClassReceiver(self: *Resolver, receiver: *const ast.Node) ?struct { name: []const u8, is_static: bool } {
    const recv_call = switch (receiver.*) {
        .CallExpr => |call| call,
        else => return null,
    };

    if (recv_call.receiver != null) return null;

    const sym = self.scope.resolveOptional(recv_call.name) orelse return null;
    if (sym.kind == .class) {
        return .{ .name = sym.name, .is_static = true };
    }

    if (sym.kind == .variable) {
        if (sym.class_name) |class_name| {
            return .{ .name = class_name, .is_static = false };
        }
    }

    return null;
}

fn findClassStmt(self: *Resolver, module: *ast.Module, class_name: []const u8) ?ast.ClassStmt {
    _ = self;
    for (module.statements) |stmt| {
        switch (stmt) {
            .ClassStmt => |class_stmt| {
                if (class_stmt.name) |name_token| {
                    if (std.mem.eql(u8, name_token.name(), class_name)) {
                        return class_stmt;
                    }
                }
            },
            else => {},
        }
    }
    return null;
}

fn formatArityList(self: *Resolver, arities: []const usize) []const u8 {
    _ = self;
    var list_buf: [64]u8 = undefined;
    var list_stream = std.io.fixedBufferStream(&list_buf);
    const list_writer = list_stream.writer();
    for (arities, 0..) |arity, i| {
        if (i > 0) list_writer.writeAll(" or ") catch @panic("Error formatting arity list");
        list_writer.print("{d}", .{arity}) catch @panic("Error formatting arity list");
    }
    return list_stream.getWritten();
}

fn receiverInferredType(self: *Resolver, receiver: *const ast.Node) ?Scope.Symbol.InferredType {
    return switch (receiver.*) {
        .StringExpr => .string,
        .NumExpr => .num,
        .BoolExpr => .bool_type,
        .ListExpr => .list,
        .MapExpr => .map,
        .CallExpr => |call| blk: {
            if (call.receiver != null) break :blk null;
            const sym = self.scope.resolveOptional(call.name) orelse break :blk null;
            break :blk sym.inferred_type;
        },
        else => null,
    };
}

fn builtinMethodArity(signature: []const u8) usize {
    const open_index = std.mem.indexOfScalar(u8, signature, '(') orelse return 0;
    const close_index = std.mem.indexOfScalarPos(u8, signature, open_index, ')') orelse return 0;
    if (close_index <= open_index + 1) return 0;

    var count: usize = 1;
    var i = open_index + 1;
    while (i < close_index) : (i += 1) {
        if (signature[i] == ',') count += 1;
    }
    return count;
}

fn containsArity(arities: []const usize, value: usize) bool {
    for (arities) |arity| {
        if (arity == value) return true;
    }
    return false;
}

fn visitAssignmentExpr(self: *Resolver, expr: ast.AssignmentExpr) void {
    self.resolveNode(expr.value);
    self.resolveNode(expr.target);
}

fn visitInfixExpr(self: *Resolver, expr: ast.InfixExpr) void {
    self.resolveNode(expr.left);
    self.resolveNode(expr.right);
}

fn visitPrefixExpr(self: *Resolver, expr: ast.PrefixExpr) void {
    self.resolveNode(expr.right);
}

fn visitGroupingExpr(self: *Resolver, expr: ast.GroupingExpr) void {
    self.resolveNode(expr.expression);
}

fn visitListExpr(self: *Resolver, expr: ast.ListExpr) void {
    for (expr.elements) |*elem| {
        self.resolveNode(elem);
    }
}

fn visitMapExpr(self: *Resolver, expr: ast.MapExpr) void {
    for (expr.entries) |*entry| {
        self.resolveNode(entry);
    }
}

fn visitSubscriptExpr(self: *Resolver, expr: ast.SubscriptExpr) void {
    self.resolveNode(expr.receiver);
    for (expr.arguments) |*arg| {
        self.resolveNode(@constCast(arg));
    }
}

fn visitSuperExpr(self: *Resolver, expr: ast.SuperExpr) void {
    for (expr.arguments) |*arg| {
        self.resolveNode(@constCast(arg));
    }
    if (expr.blockArgument.*) |*block| {
        self.resolveNode(block);
    }
}
