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

pub fn init(allocator: std.mem.Allocator, reporter: *Reporter) !Resolver {
    return .{
        .allocator = allocator,
        .scope = try Scope.init(allocator, reporter),
        .reporter = reporter,
        .class_depth = 0,
    };
}

pub fn deinit(self: *Resolver) void {
    self.scope.deinit();
}

pub fn resolve(self: *Resolver, module: *ast.Module) void {
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

    if (stmt.initializer.*) |*initializer| {
        self.resolveNode(initializer);
        inferred_type = self.inferType(initializer);
    }

    if (stmt.name) |name| {
        self.scope.declareWithType(name, .variable, inferred_type);
    }
}

fn inferType(self: *Resolver, node: *const ast.Node) ?Scope.Symbol.InferredType {
    _ = self;
    return switch (node.*) {
        .NumExpr => .num,
        .StringExpr => .string,
        .BoolExpr => .bool_type,
        .NullExpr => .null_type,
        .ListExpr => .list,
        .MapExpr => .map,
        else => null,
    };
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
