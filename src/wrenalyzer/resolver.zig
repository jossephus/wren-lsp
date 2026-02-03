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

pub const ResolvedRef = struct {
    use_token: Token,
    decl_token: Token,
    kind: Scope.Symbol.Kind,
    is_write: bool,
};

/// Information about a class method.
pub const ClassMethodInfo = struct {
    name: []const u8,
    arity: usize,
    is_static: bool,
    is_constructor: bool,
};

/// Type information for an imported symbol, provided by the LSP layer.
pub const ImportSymbolInfo = struct {
    kind: Scope.Symbol.Kind,
    inferred_type: ?Scope.Symbol.InferredType = null,
    fn_arity: ?usize = null,
    class_name: ?[]const u8 = null,
    methods: ?[]const ClassMethodInfo = null,
};

allocator: std.mem.Allocator,
scope: Scope,
reporter: *Reporter,
class_depth: usize,
module: ?*ast.Module,
refs_out: ?*std.ArrayListUnmanaged(ResolvedRef) = null,
import_symbols: ?*const std.StringHashMapUnmanaged(ImportSymbolInfo) = null,

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
        // Find constructor arity (use first constructor found, or null if none)
        const ctor_arity = self.findConstructorArity(stmt.methods);
        self.scope.declareWithType(name, .class, .class_type, ctor_arity, null);
    }

    self.scope.beginClass();
    self.class_depth += 1;

    for (stmt.methods) |*method| {
        self.resolveNode(method);
    }

    self.class_depth -= 1;
    self.scope.endClass();
}

fn findConstructorArity(self: *Resolver, methods: []ast.Node) ?usize {
    _ = self;
    for (methods) |method_node| {
        switch (method_node) {
            .Method => |method| {
                if (method.constructKeyword != null) {
                    return method.parameters.len;
                }
            },
            else => {},
        }
    }
    return null;
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
                if (self.import_symbols) |import_syms| {
                    if (import_syms.get(v.name())) |info| {
                        self.scope.declareWithType(v, info.kind, info.inferred_type, info.fn_arity, info.class_name);
                        continue;
                    }
                }
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

        // Track method name as a reference (for hover/goto on method names)
        // Method names don't have a declaration we can resolve, but we track them
        // so hover shows something useful
        if (self.refs_out) |refs| {
            refs.append(self.allocator, .{
                .use_token = expr.name,
                .decl_token = expr.name, // Point to itself since no actual decl
                .kind = .method,
                .is_write = false,
            }) catch {};
        }
    } else {
        if (self.scope.resolveOptional(expr.name)) |sym| {
            if (self.refs_out) |refs| {
                refs.append(self.allocator, .{
                    .use_token = expr.name,
                    .decl_token = sym.token,
                    .kind = sym.kind,
                    .is_write = false,
                }) catch {};
            }
        } else {
            // Report undefined symbol errors both at top-level and inside classes
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

    const call_name = expr.name.name();
    var arg_count: usize = expr.arguments.len;
    if (expr.blockArgument.* != null) {
        arg_count += 1;
    }

    if (self.findClassStmt(module, class_info.name)) |class_stmt| {
        const methods = extractMethodsFromAst(self.allocator, class_stmt.methods) orelse return;
        defer self.allocator.free(methods);

        _ = checkMethodArity(
            self,
            class_info.name,
            call_name,
            arg_count,
            class_info.is_static,
            methods,
            expr.name,
        );
        return;
    }

    if (self.import_symbols) |import_syms| {
        if (import_syms.get(class_info.name)) |import_info| {
            if (import_info.methods) |methods| {
                _ = checkMethodArity(
                    self,
                    class_info.name,
                    call_name,
                    arg_count,
                    class_info.is_static,
                    methods,
                    expr.name,
                );
            }
        }
    }
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

fn checkInfixTypes(self: *Resolver, expr: ast.InfixExpr) void {
    const left_type = self.exprInferredType(expr.left) orelse return;
    const right_type = self.exprInferredType(expr.right) orelse return;
    const operator_type = expr.operator.type;

    const is_num = left_type == .num and right_type == .num;
    const is_string = left_type == .string and right_type == .string;

    switch (operator_type) {
        .plus => {
            if (is_num or is_string) return;
            self.reportInfixTypeError(expr.operator, "+", left_type, right_type, "num or string");
        },
        .minus, .star, .slash, .percent, .caret, .amp, .pipe, .lessLess, .greaterGreater => {
            if (is_num) return;
            self.reportInfixTypeError(expr.operator, operator_type, left_type, right_type, "num");
        },
        .less, .lessEqual, .greater, .greaterEqual => {
            if (is_num or is_string) return;
            self.reportInfixTypeError(expr.operator, operator_type, left_type, right_type, "num or string");
        },
        .dotDot, .dotDotDot => {
            if (is_num) return;
            self.reportInfixTypeError(expr.operator, operator_type, left_type, right_type, "num");
        },
        .ampAmp, .pipePipe => {
            if (left_type == .bool_type and right_type == .bool_type) return;
            self.reportInfixTypeError(expr.operator, operator_type, left_type, right_type, "bool");
        },
        .equalEqual, .bangEqual => return,
        else => return,
    }
}

fn exprInferredType(self: *Resolver, node: *const ast.Node) ?Scope.Symbol.InferredType {
    return switch (node.*) {
        .NumExpr => .num,
        .StringExpr => .string,
        .BoolExpr => .bool_type,
        .NullExpr => .null_type,
        .ListExpr => .list,
        .MapExpr => .map,
        .GroupingExpr => |expr| self.exprInferredType(expr.expression),
        .PrefixExpr => |expr| self.exprInferredType(expr.right),
        .CallExpr => |expr| self.inferCallExprType(expr),
        else => null,
    };
}

fn inferCallExprType(self: *Resolver, expr: ast.CallExpr) ?Scope.Symbol.InferredType {
    if (expr.receiver != null) return null;
    if (expr.arguments.len != 0) return null;
    if (expr.blockArgument.* != null) return null;

    const sym = self.scope.resolveOptional(expr.name) orelse return null;
    if (sym.inferred_type) |inferred| return inferred;
    if (sym.kind == .class) return .class_type;
    return null;
}

fn reportInfixTypeError(
    self: *Resolver,
    operator: Token,
    operator_name: anytype,
    left_type: Scope.Symbol.InferredType,
    right_type: Scope.Symbol.InferredType,
    expected: []const u8,
) void {
    const op_name = switch (@TypeOf(operator_name)) {
        Token.Tag => @tagName(operator_name),
        []const u8 => operator_name,
        else => "operator",
    };

    var buf: [192]u8 = undefined;
    const msg = std.fmt.bufPrint(
        &buf,
        "Operator '{s}' expects {s}, found {s} and {s}",
        .{ op_name, expected, @tagName(left_type), @tagName(right_type) },
    ) catch "Invalid operand types";
    self.reporter.reportError(operator, msg);
}

fn visitAssignmentExpr(self: *Resolver, expr: ast.AssignmentExpr) void {
    self.resolveNode(expr.value);

    // Track reference to assignment target (write access)
    switch (expr.target.*) {
        .CallExpr => |call| {
            if (call.receiver == null) {
                if (self.scope.resolveOptional(call.name)) |sym| {
                    if (self.refs_out) |refs| {
                        refs.append(self.allocator, .{
                            .use_token = call.name,
                            .decl_token = sym.token,
                            .kind = sym.kind,
                            .is_write = true,
                        }) catch {};
                    }
                } else {
                    // Variable not found - report error
                    _ = self.scope.resolve(call.name);
                }
            }
            // Don't call resolveNode for CallExpr - we already tracked it
            // Just resolve the receiver if present
            if (call.receiver) |recv| {
                self.resolveNode(recv);
            }
            for (call.arguments) |*arg| {
                self.resolveNode(arg);
            }
            if (call.blockArgument.*) |*block| {
                self.resolveNode(block);
            }
            return;
        },
        else => {},
    }

    self.resolveNode(expr.target);
}

fn visitInfixExpr(self: *Resolver, expr: ast.InfixExpr) void {
    self.resolveNode(expr.left);
    self.resolveNode(expr.right);
    self.checkInfixTypes(expr);
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

pub fn extractMethodsFromAst(gpa: std.mem.Allocator, methods: []const ast.Node) ?[]const ClassMethodInfo {
    var method_count: usize = 0;
    for (methods) |method_node| {
        if (method_node == .Method) method_count += 1;
    }

    if (method_count == 0) return null;

    var result = gpa.alloc(ClassMethodInfo, method_count) catch return null;
    var idx: usize = 0;
    for (methods) |method_node| {
        switch (method_node) {
            .Method => |method| {
                if (idx >= method_count) break;
                const method_name = if (method.name) |n| n.name() else "unknown";
                result[idx] = .{
                    .name = method_name,
                    .arity = method.parameters.len,
                    .is_static = method.staticKeyword != null,
                    .is_constructor = method.constructKeyword != null,
                };
                idx += 1;
            },
            else => {},
        }
    }

    return result[0..idx];
}

fn checkMethodArity(
    self: *Resolver,
    class_name: []const u8,
    call_name: []const u8,
    arg_count: usize,
    is_static: bool,
    methods: []const ClassMethodInfo,
    error_token: Token,
) bool {
    var arities: [8]usize = undefined;
    var arity_len: usize = 0;
    var matches_arity = false;

    for (methods) |method| {
        if (!std.mem.eql(u8, method.name, call_name)) continue;

        // Check static/constructor constraints
        if (is_static) {
            // For static calls, method must be static or constructor
            if (!method.is_static and !method.is_constructor) continue;
        } else {
            // For instance calls, method must not be static or constructor
            if (method.is_static or method.is_constructor) continue;
        }

        const arity = method.arity;
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

    if (matches_arity) return true;

    if (arity_len == 0) {
        if (is_static and std.mem.eql(u8, call_name, "new")) return true;
        const kind_label = if (is_static) "static" else "instance";
        var buf: [192]u8 = undefined;
        const msg = std.fmt.bufPrint(
            &buf,
            "Unknown {s} method {s}.{s}",
            .{ kind_label, class_name, call_name },
        ) catch "Unknown method";
        self.reporter.reportError(error_token, msg);
        return true;
    }

    const kind_label = if (is_static) "static" else "instance";

    var list_buf: [64]u8 = undefined;
    var list_stream = std.io.fixedBufferStream(&list_buf);
    const list_writer = list_stream.writer();
    for (arities[0..arity_len], 0..) |arity, i| {
        if (i > 0) list_writer.writeAll(" or ") catch @panic("Error formatting arity list");
        list_writer.print("{d}", .{arity}) catch @panic("Error formatting arity list");
    }
    const arity_list = list_stream.getWritten();

    var buf: [192]u8 = undefined;
    const msg = std.fmt.bufPrint(
        &buf,
        "Expected {s} arguments for {s} {s}.{s}, found {d}",
        .{ arity_list, kind_label, class_name, call_name, arg_count },
    ) catch "Incorrect argument count";
    self.reporter.reportError(error_token, msg);
    return true;
}
