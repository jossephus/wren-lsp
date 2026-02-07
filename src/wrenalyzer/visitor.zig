//! AST Visitor - provides recursive traversal of the Wren AST.
//!
//! Usage:
//!   const MyVisitor = struct {
//!       pub fn visitCallExpr(self: *@This(), node: ast.CallExpr) void { ... }
//!       // ... other visit methods
//!   };
//!   var v = MyVisitor{};
//!   visitor.walk(&v, &node);

const std = @import("std");
const ast = @import("ast.zig");
const Node = ast.Node;

pub fn walk(v: anytype, node: *const Node) void {
    switch (node.*) {
        .Module => |mod| visitModule(v, mod),
        .AssignmentExpr => |expr| visitAssignmentExpr(v, expr),
        .InfixExpr => |expr| visitInfixExpr(v, expr),
        .PrefixExpr => |expr| visitPrefixExpr(v, expr),
        .NumExpr => |expr| visitNumExpr(v, expr),
        .NullExpr => |expr| visitNullExpr(v, expr),
        .GroupingExpr => |expr| visitGroupingExpr(v, expr),
        .ListExpr => |expr| visitListExpr(v, expr),
        .MapExpr => |expr| visitMapExpr(v, expr),
        .BoolExpr => |expr| visitBoolExpr(v, expr),
        .ThisExpr => |expr| visitThisExpr(v, expr),
        .FieldExpr => |expr| visitFieldExpr(v, expr),
        .StaticFieldExpr => |expr| visitStaticFieldExpr(v, expr),
        .StringExpr => |expr| visitStringExpr(v, expr),
        .CallExpr => |expr| visitCallExpr(v, expr),
        .Body => |body| visitBody(v, body),
        .SuperExpr => |expr| visitSuperExpr(v, expr),
        .ClassStmt => |stmt| visitClassStmt(v, stmt),
        .Method => |method| visitMethod(v, method),
        .ImportStmt => |stmt| visitImportStmt(v, stmt),
        .VarStmt => |stmt| visitVarStmt(v, stmt),
        .BreakStmt => |stmt| visitBreakStmt(v, stmt),
        .IfStmt => |stmt| visitIfStmt(v, stmt),
        .ForStmt => |stmt| visitForStmt(v, stmt),
        .WhileStmt => |stmt| visitWhileStmt(v, stmt),
        .ReturnStmt => |stmt| visitReturnStmt(v, stmt),
        .BlockStmt => |stmt| visitBlockStmt(v, stmt),
        .SubscriptExpr => |expr| visitSubscriptExpr(v, expr),
    }
}

fn visitModule(v: anytype, mod: *ast.Module) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitModule")) {
        v.visitModule(mod);
    }
    for (mod.statements) |*stmt| {
        walk(v, stmt);
    }
}

fn visitAssignmentExpr(v: anytype, expr: ast.AssignmentExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitAssignmentExpr")) {
        v.visitAssignmentExpr(expr);
    }
    walk(v, expr.target);
    walk(v, expr.value);
}

fn visitInfixExpr(v: anytype, expr: ast.InfixExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitInfixExpr")) {
        v.visitInfixExpr(expr);
    }
    walk(v, expr.left);
    walk(v, expr.right);
}

fn visitPrefixExpr(v: anytype, expr: ast.PrefixExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitPrefixExpr")) {
        v.visitPrefixExpr(expr);
    }
    walk(v, expr.right);
}

fn visitNumExpr(v: anytype, expr: ast.NumExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitNumExpr")) {
        v.visitNumExpr(expr);
    }
}

fn visitNullExpr(v: anytype, expr: ast.NullExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitNullExpr")) {
        v.visitNullExpr(expr);
    }
}

fn visitGroupingExpr(v: anytype, expr: ast.GroupingExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitGroupingExpr")) {
        v.visitGroupingExpr(expr);
    }
    walk(v, expr.expression);
}

fn visitListExpr(v: anytype, expr: ast.ListExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitListExpr")) {
        v.visitListExpr(expr);
    }
    for (expr.elements) |*elem| {
        walk(v, elem);
    }
}

fn visitMapExpr(v: anytype, expr: ast.MapExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitMapExpr")) {
        v.visitMapExpr(expr);
    }
    for (expr.entries) |*entry| {
        walk(v, entry);
    }
}

fn visitBoolExpr(v: anytype, expr: ast.BoolExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitBoolExpr")) {
        v.visitBoolExpr(expr);
    }
}

fn visitThisExpr(v: anytype, expr: ast.ThisExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitThisExpr")) {
        v.visitThisExpr(expr);
    }
}

fn visitFieldExpr(v: anytype, expr: ast.FieldExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitFieldExpr")) {
        v.visitFieldExpr(expr);
    }
}

fn visitStaticFieldExpr(v: anytype, expr: ast.StaticFieldExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitStaticFieldExpr")) {
        v.visitStaticFieldExpr(expr);
    }
}

fn visitStringExpr(v: anytype, expr: ast.StringExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitStringExpr")) {
        v.visitStringExpr(expr);
    }
}

fn visitCallExpr(v: anytype, expr: ast.CallExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitCallExpr")) {
        v.visitCallExpr(expr);
    }
    if (expr.receiver) |recv| {
        walk(v, recv);
    }
    for (expr.arguments) |*arg| {
        walk(v, @constCast(arg));
    }
    if (expr.blockArgument.*) |*block| {
        walk(v, block);
    }
}

fn visitBody(v: anytype, body: ast.Body) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitBody")) {
        v.visitBody(body);
    }
    if (body.expression) |expr| {
        walk(v, expr);
    }
    if (body.statements) |stmts| {
        for (stmts) |*stmt| {
            walk(v, stmt);
        }
    }
}

fn visitSuperExpr(v: anytype, expr: ast.SuperExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitSuperExpr")) {
        v.visitSuperExpr(expr);
    }
    for (expr.arguments) |*arg| {
        walk(v, @constCast(arg));
    }
    if (expr.blockArgument.*) |*block| {
        walk(v, block);
    }
}

fn visitClassStmt(v: anytype, stmt: ast.ClassStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitClassStmt")) {
        v.visitClassStmt(stmt);
    }
    for (stmt.methods) |*method| {
        walk(v, method);
    }
    for (stmt.vars) |*var_node| {
        walk(v, var_node);
    }
}

fn visitMethod(v: anytype, method: ast.Method) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitMethod")) {
        v.visitMethod(method);
    }
    if (method.body.*) |*body| {
        walk(v, body);
    }
}

fn visitImportStmt(v: anytype, stmt: ast.ImportStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitImportStmt")) {
        v.visitImportStmt(stmt);
    }
}

fn visitVarStmt(v: anytype, stmt: ast.VarStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitVarStmt")) {
        v.visitVarStmt(stmt);
    }
    if (stmt.initializer.*) |*init| {
        walk(v, init);
    }
}

fn visitBreakStmt(v: anytype, stmt: ast.BreakStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitBreakStmt")) {
        v.visitBreakStmt(stmt);
    }
}

fn visitIfStmt(v: anytype, stmt: ast.IfStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitIfStmt")) {
        v.visitIfStmt(stmt);
    }
    walk(v, stmt.condition);
    walk(v, stmt.thenBranch);
    if (stmt.elseBranch) |else_branch| {
        walk(v, else_branch);
    }
}

fn visitForStmt(v: anytype, stmt: ast.ForStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitForStmt")) {
        v.visitForStmt(stmt);
    }
    walk(v, stmt.iterator);
    walk(v, stmt.body);
}

fn visitWhileStmt(v: anytype, stmt: ast.WhileStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitWhileStmt")) {
        v.visitWhileStmt(stmt);
    }
    walk(v, stmt.condition);
    walk(v, stmt.body);
}

fn visitReturnStmt(v: anytype, stmt: ast.ReturnStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitReturnStmt")) {
        v.visitReturnStmt(stmt);
    }
    if (stmt.value.*) |*val| {
        walk(v, val);
    }
}

fn visitBlockStmt(v: anytype, stmt: ast.BlockStmt) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitBlockStmt")) {
        v.visitBlockStmt(stmt);
    }
    for (stmt.statements) |*s| {
        walk(v, s);
    }
}

fn visitSubscriptExpr(v: anytype, expr: ast.SubscriptExpr) void {
    if (comptime hasMethod(@TypeOf(v.*), "visitSubscriptExpr")) {
        v.visitSubscriptExpr(expr);
    }
    walk(v, expr.receiver);
    for (expr.arguments) |*arg| {
        walk(v, @constCast(arg));
    }
}

fn hasMethod(comptime T: type, comptime name: []const u8) bool {
    return @hasDecl(T, name);
}
