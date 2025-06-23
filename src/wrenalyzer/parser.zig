const std = @import("std");
const Lexer = @import("lexer.zig");
pub const Parser = @This();
const token = @import("token.zig");
const Token = token.Token;
const Tag = Token.Tag;
const ast = @import("ast.zig");
const Module = ast.Module;
const Node = ast.Node;

lexer: Lexer,
allocator: std.mem.Allocator,
current: ?Token,
previous: ?Token,

pub fn new(allocator: std.mem.Allocator, lexer: Lexer) !Parser {
    return .{
        .allocator = allocator,
        .lexer = lexer,
        .current = null,
        .previous = null,
    };
}

pub fn parseModule(self: *Parser) !Module {
    self.ignoreLine();

    var statements = std.ArrayList(Node).init(self.allocator);
    while (self.peek() != Tag.eof) {
        try statements.append(self.definition());

        if (self.peek() == Tag.eof) break;
        self.consumeLine();
    }
    return Module.init(try statements.toOwnedSlice());
}

pub fn definition(self: *Parser) ast.Node {
    if (self.match(Tag.classKeyword) != null) return self.finishClass(null);

    if (self.match(Tag.foreignKeyword) != null) {
        const foreignKeyword = self.previous;
        _ = self.consume(Tag.classKeyword);
        return self.finishClass(foreignKeyword);
    }

    if (self.match(Tag.importKeyword) != null) {
        const path = self.consume(Tag.string);
        var variables: ?std.ArrayList(?Token) = null;

        if (self.match(Tag.forKeyword) != null) {
            self.ignoreLine();

            variables = std.ArrayList(?Token).init(self.allocator);

            while (true) {
                variables.?.append(self.consume(Tag.name)) catch @panic("Error allocating memory");
                if (self.match(Tag.comma) == null) break;
                self.ignoreLine();
            }
        }

        return .{ .ImportStmt = ast.ImportStmt.init(path, if (variables != null) variables.?.toOwnedSlice() catch @panic("Error allocating memory") else null) };
    }

    if (self.match(Tag.varKeyword) != null) {
        const name = self.consume(Tag.name);
        var initializer: ?ast.Node = null;

        if (self.match(Tag.equal) != null) {
            initializer = self.expression();
        }

        return .{ .VarStmt = ast.VarStmt.init(name, &initializer) };
    }

    return self.statement();
}

pub fn statement(self: *Parser) ast.Node {
    if (self.match(Tag.breakKeyword)) |_| return .{ .BreakStmt = ast.BreakStmt.init(self.previous) };

    if (self.match(Tag.ifKeyword)) |_| return self.ifStatement();

    if (self.match(Tag.forKeyword)) |_| return self.forStatement();

    if (self.match(Tag.whileKeyword) != null) return self.whileStatement();

    if (self.match(Tag.returnKeyword) != null) {
        const keyword = self.previous;
        var value: ?ast.Node = null;
        if (self.peek() != Tag.line) {
            value = self.expression();
        }
        return .{ .ReturnStmt = ast.ReturnStmt.init(keyword, &value) };
    }

    if (self.match(Tag.leftBrace)) |_| return self.blockStatement();

    return self.expression();
}

pub fn expression(self: *Parser) ast.Node {
    return self.assignment();
}

pub fn ifStatement(self: *Parser) ast.Node {
    _ = self.consume(Tag.leftParen);
    self.ignoreLine();

    const condition = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
    condition.* = self.expression();

    _ = self.consume(Tag.rightParen);

    const thenBranch = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
    thenBranch.* = self.statement();

    var elseBranch: ?*ast.Node = null;

    if (self.match(Tag.elseKeyword)) |_| {
        const elsePtr = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
        elsePtr.* = self.statement();
        elseBranch = elsePtr;
    }
    return .{ .IfStmt = ast.IfStmt.init(condition, thenBranch, elseBranch) };
}

pub fn whileStatement(self: *Parser) ast.Node {
    _ = self.consume(Tag.leftParen);
    self.ignoreLine();

    const condition = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
    condition.* = self.expression();

    _ = self.consume(Tag.rightParen);
    var body = self.statement();

    return .{ .WhileStmt = ast.WhileStmt.init(condition, &body) };
}

pub fn blockStatement(self: *Parser) ast.Node {
    var statements = std.ArrayList(ast.Node).init(self.allocator);
    self.ignoreLine();

    while (self.peek() != Tag.rightBrace and self.peek() != Tag.eof) {
        statements.append(self.definition()) catch @panic("Error allocating memory");
        if (self.peek() == Tag.rightBrace) break;

        self.consumeLine();
    }
    _ = self.consume(Tag.rightBrace);

    return .{ .BlockStmt = ast.BlockStmt.init(statements.toOwnedSlice() catch @panic("Error allocating memory")) };
}

pub fn forStatement(self: *Parser) ast.Node {
    _ = self.consume(Tag.leftParen);
    const variable = self.consume(Tag.name);
    _ = self.consume(Tag.inKeyword);

    self.ignoreLine();
    var iterator = self.expression();

    _ = self.consume(Tag.rightParen);
    var body = self.statement();

    return .{ .ForStmt = ast.ForStmt.init(variable, &iterator, &body) };
}

pub fn assignment(self: *Parser) ast.Node {
    var expr = self.conditional();
    if (self.match(Tag.equal) == null) return expr;

    const equal = self.previous;
    var value = self.assignment();
    return ast.Node{ .AssignmentExpr = ast.AssignmentExpr.init(&expr, equal.?, &value) };
}

pub fn conditional(self: *Parser) ast.Node {
    const expr = self.logicalOr();
    return expr;
}

pub fn logicalOr(self: *Parser) ast.Node {
    const expr = self.parseInfix(&[_]Tag{Tag.pipePipe}, logicalAnd);
    return expr;
}

pub fn logicalAnd(self: *Parser) ast.Node {
    const expr = self.parseInfix(&[_]Tag{Tag.ampAmp}, equality);
    return expr;
}

const EQUALITY_OPERATORS = &[_]Tag{ Tag.equalEqual, Tag.bangEqual };

pub fn equality(self: *Parser) ast.Node {
    const expr = self.parseInfix(EQUALITY_OPERATORS, typeTest);
    return expr;
}

const COMPARISON_OPERATORS = &[_]Tag{
    Tag.less,
    Tag.lessEqual,
    Tag.greater,
    Tag.greaterEqual,
};

const BITWISE_SHIFT_OPERATORS = &[_]Tag{
    Tag.lessLess,
    Tag.greaterGreater,
};

const RANGE_OPERATORS = &[_]Tag{
    Tag.dotDot,
    Tag.dotDotDot,
};

const TERM_OPERATORS = &[_]Tag{
    Tag.plus,
    Tag.minus,
};

const FACTOR_OPERATORS = &[_]Tag{
    Tag.star,
    Tag.slash,
    Tag.percent,
};

const PREFIX_OPERATORS = &[_]Tag{ Tag.minus, Tag.bang, Tag.tilde };

const INFIX_OPERATORS = &[_]Tag{
    Tag.pipePipe,
    Tag.ampAmp,
    Tag.equalEqual,
    Tag.bangEqual,
    Tag.isKeyword,
    Tag.less,
    Tag.lessEqual,
    Tag.greater,
    Tag.greaterEqual,
    Tag.pipe,
    Tag.caret,
    Tag.amp,
    Tag.lessLess,
    Tag.greaterGreater,
    Tag.dotDot,
    Tag.dotDotDot,
    Tag.plus,
    Tag.minus,
    Tag.star,
    Tag.slash,
    Tag.percent,
};

pub fn typeTest(self: *Parser) ast.Node {
    const expr = self.parseInfix(&[_]Tag{Tag.isKeyword}, comparison);
    return expr;
}

pub fn comparison(self: *Parser) ast.Node {
    const expr = self.parseInfix(COMPARISON_OPERATORS, bitwiseOr);
    return expr;
}

pub fn bitwiseOr(self: *Parser) ast.Node {
    const expr = self.parseInfix(&[_]Tag{Tag.pipe}, bitwiseXor);
    return expr;
}

pub fn bitwiseXor(self: *Parser) ast.Node {
    const expr = self.parseInfix(&[_]Tag{Tag.caret}, bitwiseAnd);
    return expr;
}

pub fn bitwiseAnd(self: *Parser) ast.Node {
    const expr = self.parseInfix(&[_]Tag{Tag.amp}, bitwiseShift);
    return expr;
}

pub fn bitwiseShift(self: *Parser) ast.Node {
    const expr = self.parseInfix(BITWISE_SHIFT_OPERATORS, range);
    return expr;
}

pub fn range(self: *Parser) ast.Node {
    const expr = self.parseInfix(RANGE_OPERATORS, term);
    return expr;
}

pub fn term(self: *Parser) ast.Node {
    const expr = self.parseInfix(TERM_OPERATORS, factor);
    return expr;
}

pub fn factor(self: *Parser) ast.Node {
    const expr = self.parseInfix(FACTOR_OPERATORS, prefix);
    return expr;
}

fn prefix(self: *Parser) ast.Node {
    if (self.matchAny(PREFIX_OPERATORS) != null) {
        const pre = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
        pre.* = self.prefix();
        const node = ast.Node{ .PrefixExpr = ast.PrefixExpr.init(self.previous.?, pre) };
        return node;
    }
    return self.call();
}

fn call(self: *Parser) ast.Node {
    const expr = self.primary();

    //while (true) {}
    return expr;
}

fn methodCall(self: *Parser, receiver: ?*ast.Node, name: Token) ast.Node {
    const arguments = self.finishCall();
    var blockArgument = arguments.blockArgument;
    return .{ .CallExpr = ast.CallExpr.init(receiver, name, arguments.arguments, &blockArgument) };
}

fn finishClass(self: *Parser, foreignKeyword: ?Token) ast.Node {
    _ = .{ self, foreignKeyword };
    const name = self.consume(Tag.name);

    var superClass: ?Token = null;
    if (self.match(Tag.isKeyword) != null) {
        superClass = self.consume(Tag.name);
    }

    var methods = std.ArrayList(ast.Node).init(self.allocator);

    _ = self.consume(Tag.leftBrace);
    self.ignoreLine();

    while (self.match(Tag.rightBrace) == null and self.peek() != Tag.eof) {
        methods.append(self.method()) catch @panic("Error allocating memory");

        if (self.match(Tag.rightBrace) != null) break;

        self.consumeLine();
    }
    return .{ .ClassStmt = ast.ClassStmt.init(foreignKeyword, name, superClass, methods.toOwnedSlice() catch @panic("Error allocating memory")) };
}

fn method(self: *Parser) ast.Node {
    var foreignKeyword: ?Token = null;

    if (self.match(Tag.foreignKeyword) != null) {
        foreignKeyword = self.previous;
    }

    var staticKeyword: ?Token = null;
    if (self.match(Tag.staticKeyword) != null) {
        staticKeyword = self.previous;
    }

    var constructKeyword: ?Token = null;
    if (self.match(Tag.constructKeyword) != null) {
        constructKeyword = self.previous;
    }

    var name: ?Token = null;
    var parameters: []Token = &[_]Token{};
    var allowParameters: bool = false;

    if (self.match(Tag.leftBracket) != null) {
        parameters = self.parameterList();
        _ = self.consume(Tag.rightBracket);
        allowParameters = false;
    } else if (self.matchAny(&[_]Tag{ Tag.bang, Tag.tilde }) != null) {
        allowParameters = false;
    } else {
        _ = self.consume(Tag.name);
        allowParameters = true;
    }
    name = self.previous;

    if (self.match(Tag.leftParen) != null) {
        if (!allowParameters) {
            self.err("Expecting parameters\n");
        }

        self.ignoreLine();

        if (self.match(Tag.rightParen) == null) {
            parameters = self.parameterList();
            self.ignoreLine();
            _ = self.consume(Tag.rightParen);
        }
    }

    var body: ?ast.Node = null;
    if (foreignKeyword == null) {
        _ = self.consume(Tag.leftBrace);
        body = self.finishBody(parameters);
    }

    return .{ .Method = ast.Method.init(foreignKeyword, staticKeyword, constructKeyword, name, parameters, &body) };
}

fn finishCall(self: *Parser) struct {
    arguments: []const ast.Node,
    blockArgument: ?ast.Node,
} {
    var parameters: []Token = &[_]Token{};

    const arguments = blk: {
        if (self.match(Tag.leftParen) != null) {
            if (self.match(Tag.rightParen) != null) {
                break :blk &[_]ast.Node{};
            }

            const args = self.argumentsList();
            _ = self.consume(Tag.rightParen);
            break :blk args;
        }

        break :blk &[_]ast.Node{};
    };

    const blockArgument = blk: {
        if (self.match(Tag.leftBrace) != null) {
            if (self.match(Tag.pipe) != null) {
                parameters = self.parameterList();
                _ = self.consume(Tag.pipe);
            }
            break :blk self.finishBody(parameters);
        }
        break :blk null;
    };
    return .{
        .arguments = arguments,
        .blockArgument = blockArgument,
    };
}

fn finishBody(self: *Parser, parameters: []Token) Node {
    if (self.match(Tag.rightBrace) != null) return .{ .Body = ast.Body.init(parameters, null, &[_]ast.Node{}) };

    if (!self.matchLine()) {
        var expr = self.expression();
        self.ignoreLine();
        _ = self.consume(Tag.rightBrace);
        return .{ .Body = ast.Body.init(parameters, &expr, null) };
    }

    if (self.match(Tag.rightBrace) != null) return .{ .Body = ast.Body.init(parameters, null, &[_]ast.Node{}) };

    var statements = std.ArrayList(ast.Node).init(self.allocator);

    while (self.peek() != Tag.eof) {
        statements.append(self.definition()) catch @panic("Finish Body: appending failed");
        self.consumeLine();

        if (self.match(Tag.rightBrace) != null) break;
    }

    return .{ .Body = ast.Body.init(parameters, null, statements.toOwnedSlice() catch @panic("finishBody toOwnedSlice error")) };
}

fn argumentsList(self: *Parser) []ast.Node {
    var arguments = std.ArrayList(ast.Node).init(self.allocator);

    self.ignoreLine();

    while (true) {
        arguments.append(self.expression()) catch @panic("Memory access limited");
        if (self.match(Tag.comma) == null) break;
        self.ignoreLine();
    }
    return arguments.toOwnedSlice() catch @panic("argumentsList Panic");
}

fn parameterList(self: *Parser) []Token {
    var arguments = std.ArrayList(Token).init(self.allocator);

    self.ignoreLine();

    while (true) {
        arguments.append(self.consume(Tag.name).?) catch @panic("Memory access limited");
        if (self.match(Tag.comma) != null) break;
        self.ignoreLine();
    }
    return arguments.toOwnedSlice() catch @panic("parameterList Panic");
}

fn superCall(self: *Parser) ast.Node {
    var name: ?Token = null;

    if (self.match(Tag.dot) != null) {
        name = self.consume(Tag.name);
    }

    var arguments = self.finishCall();
    return .{ .SuperExpr = ast.SuperExpr.init(name, arguments.arguments, &arguments.blockArgument) };
}

fn primary(self: *Parser) ast.Node {
    if (self.match(Tag.leftParen) != null) return self.grouping();
    if (self.match(Tag.leftBracket) != null) return self.listLiteral();
    if (self.match(Tag.leftBrace) != null) return self.mapLiteral();
    if (self.match(Tag.name) != null) return self.methodCall(null, self.previous.?);
    if (self.match(Tag.superKeyword) != null) return self.superCall();

    if (self.match(Tag.trueKeyword) != null) return .{ .BoolExpr = ast.BoolExpr.init(true) };
    if (self.match(Tag.falseKeyword) != null) return .{ .BoolExpr = ast.BoolExpr.init(false) };
    if (self.match(Tag.nullKeyword) != null) return .{ .NullExpr = ast.NullExpr.init(self.previous.?) };
    if (self.match(Tag.thisKeyword) != null) return .{ .ThisExpr = ast.ThisExpr.init(self.previous.?) };

    if (self.match(Tag.field) != null) return .{ .FieldExpr = ast.FieldExpr.init(self.previous.?) };
    if (self.match(Tag.staticField) != null) return .{ .StaticFieldExpr = ast.StaticFieldExpr.init(self.previous.?) };

    if (self.match(Tag.number) != null) return .{ .NumExpr = ast.NumExpr.init(self.previous.?) };
    if (self.match(Tag.string) != null) return .{ .StringExpr = ast.StringExpr.init(self.previous.?) };

    self.err("Expect Expression\n");

    return .{ .NullExpr = ast.NullExpr.init(self.previous.?) };
}

fn grouping(self: *Parser) ast.Node {
    const leftParent = self.previous.?;
    const expr = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
    expr.* = self.expression();
    const rightParen = self.consume(Tag.rightParen);
    return ast.Node{ .GroupingExpr = ast.GroupingExpr.init(leftParent, expr, rightParen.?) };
}

fn listLiteral(self: *Parser) ast.Node {
    const leftBracket = self.previous.?;
    var elements = std.ArrayList(Node).init(self.allocator);

    self.ignoreLine();
    while (self.peek() != Tag.rightBracket) {
        const expr = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
        expr.* = self.expression();
        std.debug.print("listLiteral {any} \n", .{expr});
        elements.append(expr.*) catch @panic("Error appending to list");

        self.ignoreLine();

        if (self.match(Tag.comma) == null) break;

        self.ignoreLine();
    }

    const rightBracket = self.consume(Tag.rightBracket);
    return ast.Node{ .ListExpr = ast.ListExpr.init(leftBracket, elements.toOwnedSlice() catch @panic("Error allocating memory"), rightBracket.?) };
}

fn mapLiteral(self: *Parser) ast.Node {
    const leftBrace = self.previous.?;
    var entries = std.ArrayList(Node).init(self.allocator);

    self.ignoreLine();
    while (self.peek() != Tag.rightBrace) {
        const key = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
        key.* = self.expression();
        entries.append(key.*) catch @panic("Error appending to list");

        self.ignoreLine();

        if (self.match(Tag.colon) == null) break;

        self.ignoreLine();

        const value = self.allocator.create(ast.Node) catch @panic("Error allocating memory");
        value.* = self.expression();
        entries.append(value.*) catch @panic("Error appending to list");

        self.ignoreLine();

        if (self.match(Tag.comma) == null) break;

        self.ignoreLine();
    }

    const rightBrace = self.consume(Tag.rightBrace);
    return ast.Node{ .MapExpr = ast.MapExpr.init(leftBrace, entries.toOwnedSlice() catch @panic("Error allocating memory"), rightBrace.?) };
}

pub fn ignoreLine(self: *Parser) void {
    _ = self.matchLine();
}

pub fn matchLine(self: *Parser) bool {
    if (self.match(Tag.line) == null) return false;

    while (self.match(Tag.line) != null) {
        // do nothing
    }

    return true;
}

pub fn match(self: *Parser, tag: Tag) ?Token {
    if (self.peek() != tag) return null;

    return self.consume(tag);
}

pub fn matchAny(self: *Parser, tags: []const Tag) ?Token {
    for (tags) |tag| {
        const result = self.match(tag);
        if (result != null) return result;
    }
    return null;
}

pub fn peek(self: *Parser) Tag {
    if (self.current == null) self.current = self.lexer.readToken() catch @panic("Unexpected error");
    return self.current.?.type;
}

fn consumeLine(self: *Parser) void {
    _ = self.consume(Tag.line);
    self.ignoreLine();
}

fn consume(self: *Parser, tag: Tag) ?Token {
    const tok = self.consumeToken();
    if (tok.?.type != tag) {
        std.debug.print("Expected: {any}, found: {any}", .{ tag, tok.?.type });
        self.err("SOON!!");
    }

    return tok;
}

pub fn consumeToken(self: *Parser) ?Token {
    _ = self.peek();
    self.previous = self.current;
    self.current = null;
    return self.previous;
}

pub fn err(self: *Parser, comptime message: []const u8) void {
    _ = self;
    std.debug.print("error: {s}\n", .{message});
}

pub fn parseInfix(self: *Parser, types: []const Tag, parseOperands: fn (parser: *Parser) ast.Node) ast.Node {
    var expr = parseOperands(self);

    while (self.matchAny(types) != null) {
        const operator = self.previous;
        var right = parseOperands(self);
        expr = ast.Node{ .InfixExpr = ast.InfixExpr.init(&expr, operator.?, &right) };
    }

    return expr;
}
