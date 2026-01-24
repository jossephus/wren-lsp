const std = @import("std");
const Lexer = @import("lexer.zig");
pub const Parser = @This();
const token = @import("token.zig");
const Token = token.Token;
const Tag = Token.Tag;
const ast = @import("ast.zig");
const Module = ast.Module;
const Node = ast.Node;
const Reporter = @import("reporter.zig");

pub const Error = struct {
    message: []const u8,
    token: token.Token,
};

lexer: Lexer,
allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
current: ?Token,
next: ?Token,
previous: ?Token,
errors: std.ArrayListUnmanaged(Error),
reporter: ?*Reporter,

pub fn new(allocator: std.mem.Allocator, lexer: Lexer) !Parser {
    return newWithReporter(allocator, lexer, null);
}

pub fn newWithReporter(allocator: std.mem.Allocator, lexer: Lexer, reporter: ?*Reporter) !Parser {
    return .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .lexer = lexer,
        .current = null,
        .next = null,
        .previous = null,
        .errors = .empty,
        .reporter = reporter,
    };
}

pub fn deinit(self: *Parser) void {
    self.lexer.deinit();
    self.arena.deinit();
    // errors are allocated via arena, so they're freed above
}

fn alloc(self: *Parser) std.mem.Allocator {
    return self.arena.allocator();
}

pub fn parseModule(self: *Parser) !Module {
    self.ignoreLine();

    var statements: std.ArrayListUnmanaged(Node) = .empty;
    var iterations: usize = 0;
    const max_iterations: usize = 100000;

    while (self.peek() != Tag.eof) {
        iterations += 1;
        if (iterations > max_iterations) {
            return error.ParseTimeout;
        }

        try statements.append(self.alloc(), self.definition());

        if (self.peek() == Tag.eof) break;
        self.consumeLine();
    }
    return Module.init(try statements.toOwnedSlice(self.alloc()));
}

pub fn definition(self: *Parser) ast.Node {
    while (self.match(Tag.hash) != null or self.match(Tag.hashBang) != null) {
        _ = self.consume(Tag.name, "Expect attribute name.");
        if (self.match(Tag.equal) != null) {
            _ = self.expression();
        }
        if (self.match(Tag.leftParen) != null) {
            while (self.peek() != Tag.rightParen and self.peek() != Tag.eof) {
                _ = self.consume(Tag.name, "Expect attribute key.");
                if (self.match(Tag.equal) != null) {
                    _ = self.expression();
                }
                if (self.match(Tag.comma) == null) break;
            }
            _ = self.consume(Tag.rightParen, "Expect ')' after attribute.");
        }
        self.ignoreLine();
    }

    if (self.match(Tag.classKeyword) != null) return self.finishClass(null);

    if (self.match(Tag.foreignKeyword) != null) {
        const foreignKeyword = self.previous;
        _ = self.consume(Tag.classKeyword, "Expect 'class' after 'foriegn'");
        return self.finishClass(foreignKeyword);
    }

    if (self.match(Tag.importKeyword) != null) {
        self.ignoreLine();
        const path = self.consume(Tag.string, "Expect import path.");
        var variables: ?std.ArrayListUnmanaged(?Token) = null;

        if (self.match(Tag.forKeyword) != null) {
            self.ignoreLine();

            variables = .empty;

            while (true) {
                variables.?.append(self.alloc(), self.consume(Tag.name, "Expect imported variable name.")) catch @panic("Error allocating memory");
                if (self.match(Tag.asKeyword) != null) {
                    variables.?.append(self.alloc(), self.consume(Tag.name, "Expect alias name after 'as'.")) catch @panic("Error allocating memory");
                }
                if (self.match(Tag.comma) == null) break;
                self.ignoreLine();
            }
        }

        return .{ .ImportStmt = ast.ImportStmt.init(path, if (variables != null) variables.?.toOwnedSlice(self.alloc()) catch @panic("Error allocating memory") else null) };
    }

    if (self.match(Tag.varKeyword) != null) {
        const name = self.consume(Tag.name, "Expect variable name.");
        const initializer = self.alloc().create(?ast.Node) catch @panic("Error allocating memory");
        initializer.* = null;

        if (self.match(Tag.equal) != null) {
            self.ignoreLine();
            initializer.* = self.expression();
        }

        return .{ .VarStmt = ast.VarStmt.init(name, initializer) };
    }

    return self.statement();
}

pub fn statement(self: *Parser) ast.Node {
    if (self.match(Tag.breakKeyword)) |_| return .{ .BreakStmt = ast.BreakStmt.init(self.previous) };
    if (self.match(Tag.continueKeyword)) |_| return .{ .ContinueStmt = ast.ContinueStmt.init(self.previous) };

    if (self.match(Tag.ifKeyword)) |_| return self.ifStatement();

    if (self.match(Tag.forKeyword)) |_| return self.forStatement();

    if (self.match(Tag.whileKeyword) != null) return self.whileStatement();

    if (self.match(Tag.returnKeyword) != null) {
        const keyword = self.previous;
        const value = self.alloc().create(?ast.Node) catch @panic("Error allocating memory");
        value.* = null;
        if (self.peek() != Tag.line) {
            value.* = self.expression();
        }
        return .{ .ReturnStmt = ast.ReturnStmt.init(keyword, value) };
    }

    if (self.match(Tag.leftBrace)) |_| return self.blockStatement();

    return self.expression();
}

pub fn expression(self: *Parser) ast.Node {
    return self.assignment();
}

pub fn ifStatement(self: *Parser) ast.Node {
    _ = self.consume(Tag.leftParen, "Expect '(' after 'if'.");
    self.ignoreLine();

    const condition = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    condition.* = self.expression();

    _ = self.consume(Tag.rightParen, "Expect ')' after if condition.");

    const thenBranch = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    thenBranch.* = self.statement();

    var elseBranch: ?*ast.Node = null;

    if (self.match(Tag.elseKeyword)) |_| {
        const elsePtr = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        elsePtr.* = self.statement();
        elseBranch = elsePtr;
    }
    return .{ .IfStmt = ast.IfStmt.init(condition, thenBranch, elseBranch) };
}

pub fn whileStatement(self: *Parser) ast.Node {
    _ = self.consume(Tag.leftParen, "Expect '(' after 'while'.");
    self.ignoreLine();

    const condition = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    condition.* = self.expression();

    _ = self.consume(Tag.rightParen, "Expect ')' after while condition.");
    const body = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    body.* = self.statement();

    return .{ .WhileStmt = ast.WhileStmt.init(condition, body) };
}

pub fn blockStatement(self: *Parser) ast.Node {
    var statements: std.ArrayListUnmanaged(ast.Node) = .empty;
    self.ignoreLine();

    while (self.peek() != Tag.rightBrace and self.peek() != Tag.eof) {
        statements.append(self.alloc(), self.definition()) catch @panic("Error allocating memory");
        if (self.peek() == Tag.rightBrace) break;

        self.consumeLine();
    }
    _ = self.consume(Tag.rightBrace, "Expect '}' after block.");

    return .{ .BlockStmt = ast.BlockStmt.init(statements.toOwnedSlice(self.alloc()) catch @panic("Error allocating memory")) };
}

pub fn forStatement(self: *Parser) ast.Node {
    _ = self.consume(Tag.leftParen, "Expect '(' after 'for'.");
    const variable = self.consume(Tag.name, "Expect for loop variable name.");
    _ = self.consume(Tag.inKeyword, "Expect 'in' after loop variable.");

    self.ignoreLine();
    const iterator = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    iterator.* = self.expression();

    _ = self.consume(Tag.rightParen, "Expect ')' after loop expression.");
    const body = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    body.* = self.statement();

    return .{ .ForStmt = ast.ForStmt.init(variable, iterator, body) };
}

pub fn assignment(self: *Parser) ast.Node {
    const expr = self.conditional();
    if (self.match(Tag.equal) == null) return expr;

    const equal = self.previous;
    const target = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    target.* = expr;
    const value = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    value.* = self.assignment();
    return ast.Node{ .AssignmentExpr = ast.AssignmentExpr.init(target, equal.?, value) };
}

pub fn conditional(self: *Parser) ast.Node {
    var expr = self.logicalOr();

    if (self.match(Tag.question) != null) {
        const questionToken = self.previous;
        self.ignoreLine();
        const thenBranch = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        thenBranch.* = self.expression();
        self.ignoreLine();
        _ = self.consume(Tag.colon, "Expect ':' after then branch of conditional operator.");
        self.ignoreLine();
        const elseBranch = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        elseBranch.* = self.conditional();

        const condition = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        condition.* = expr;

        const thenElse = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        thenElse.* = .{ .InfixExpr = ast.InfixExpr.init(thenBranch, self.previous.?, elseBranch) };

        expr = .{ .InfixExpr = ast.InfixExpr.init(condition, questionToken.?, thenElse) };
    }

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
        const pre = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        pre.* = self.prefix();
        const node = ast.Node{ .PrefixExpr = ast.PrefixExpr.init(self.previous.?, pre) };
        return node;
    }
    return self.call();
}

fn call(self: *Parser) ast.Node {
    var expr = self.primary();

    while (true) {
        // Allow method chaining across newlines (e.g., `foo\n  .bar()`)
        // Only continue on newline if followed by `.` (not `[`, as `[` starts new list literals)
        if (self.peek() == Tag.line) {
            const next = self.peekNext();
            if (next == Tag.dot) {
                self.ignoreLine();
            }
        }
        if (self.match(Tag.leftBracket) != null) {
            const leftBracket = self.previous;
            const arguments = self.argumentsList();
            self.ignoreLine();
            const rightBracket = self.consume(Tag.rightBracket, "Expect ']' after subscript arguments");
            const receiver = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
            receiver.* = expr;
            expr = .{ .SubscriptExpr = ast.SubscriptExpr.init(receiver, leftBracket, arguments, rightBracket) };
        } else if (self.match(Tag.dot) != null) {
            self.ignoreLine();
            const name = self.consume(Tag.name, "Expect method name after '.'.");
            const receiver = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
            receiver.* = expr;
            expr = self.methodCall(receiver, name.?);
        } else {
            break;
        }
    }

    return expr;
}

fn methodCall(self: *Parser, receiver: ?*ast.Node, name: Token) ast.Node {
    const arguments = self.finishCall();
    const blockArgument = self.alloc().create(?ast.Node) catch @panic("Error allocating memory");
    blockArgument.* = arguments.blockArgument;
    return .{ .CallExpr = ast.CallExpr.init(receiver, name, arguments.arguments, blockArgument) };
}

fn finishClass(self: *Parser, foreignKeyword: ?Token) ast.Node {
    _ = .{ self, foreignKeyword };
    const name = self.consume(Tag.name, "Expect class name");

    var superClass: ?Token = null;
    if (self.match(Tag.isKeyword) != null) {
        superClass = self.consume(Tag.name, "Expect name of superclass");
    }

    var methods: std.ArrayListUnmanaged(ast.Node) = .empty;

    _ = self.consume(Tag.leftBrace, "Expect '{' after class name.");
    self.ignoreLine();

    while (self.match(Tag.rightBrace) == null and self.peek() != Tag.eof) {
        methods.append(self.alloc(), self.method()) catch @panic("Error allocating memory");

        if (self.match(Tag.rightBrace) != null) break;

        self.consumeLine();
    }
    return .{ .ClassStmt = ast.ClassStmt.init(foreignKeyword, name, superClass, methods.toOwnedSlice(self.alloc()) catch @panic("Error allocating memory")) };
}

fn method(self: *Parser) ast.Node {
    while (self.match(Tag.hash) != null or self.match(Tag.hashBang) != null) {
        _ = self.consume(Tag.name, "Expect attribute name.");
        if (self.match(Tag.equal) != null) {
            _ = self.expression();
        }
        if (self.match(Tag.leftParen) != null) {
            while (self.peek() != Tag.rightParen and self.peek() != Tag.eof) {
                _ = self.consume(Tag.name, "Expect attribute key.");
                if (self.match(Tag.equal) != null) {
                    _ = self.expression();
                }
                if (self.match(Tag.comma) == null) break;
            }
            _ = self.consume(Tag.rightParen, "Expect ')' after attribute.");
        }
        self.ignoreLine();
    }

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

    const OPERATOR_METHODS = &[_]Tag{
        Tag.bang,
        Tag.tilde,
        Tag.minus,
        Tag.plus,
        Tag.star,
        Tag.slash,
        Tag.percent,
        Tag.less,
        Tag.lessEqual,
        Tag.greater,
        Tag.greaterEqual,
        Tag.equalEqual,
        Tag.bangEqual,
        Tag.amp,
        Tag.pipe,
        Tag.isKeyword,
        Tag.caret,
        Tag.lessLess,
        Tag.greaterGreater,
        Tag.dotDot,
        Tag.dotDotDot,
    };

    if (self.match(Tag.leftBracket) != null) {
        parameters = self.parameterList();
        _ = self.consume(Tag.rightBracket, "Expect ']' after parameters.");
        allowParameters = self.match(Tag.equal) != null;
    } else if (self.matchAny(OPERATOR_METHODS) != null) {
        allowParameters = true;
    } else {
        _ = self.consume(Tag.name, "Expect method name.");
        allowParameters = true;
    }
    name = self.previous;

    if (self.match(Tag.equal) != null) {
        if (self.match(Tag.leftParen) != null) {
            self.ignoreLine();
            if (self.match(Tag.rightParen) == null) {
                parameters = self.parameterList();
                self.ignoreLine();
                _ = self.consume(Tag.rightParen, "Expect ')' after setter parameter.");
            }
        }
    } else if (self.match(Tag.leftParen) != null) {
        if (!allowParameters) {
            self.err("Expecting parameters\n");
        }

        self.ignoreLine();

        if (self.match(Tag.rightParen) == null) {
            parameters = self.parameterList();
            self.ignoreLine();
            _ = self.consume(Tag.rightParen, "Expect ')' after parameters.");
        }
    }

    const body = self.alloc().create(?ast.Node) catch @panic("Error allocating memory");
    body.* = null;
    if (foreignKeyword == null) {
        _ = self.consume(Tag.leftBrace, "Expect '{' before method body.");
        body.* = self.finishBody(parameters);
    }

    return .{ .Method = ast.Method.init(foreignKeyword, staticKeyword, constructKeyword, name, parameters, body) };
}

fn finishCall(self: *Parser) struct {
    arguments: []const ast.Node,
    blockArgument: ?ast.Node,
} {
    var parameters: []Token = &[_]Token{};

    const arguments = blk: {
        if (self.match(Tag.leftParen) != null) {
            self.ignoreLine();
            if (self.match(Tag.rightParen) != null) {
                break :blk &[_]ast.Node{};
            }

            const args = self.argumentsList();
            self.ignoreLine();
            _ = self.consume(Tag.rightParen, "Expect ')' after arguments.");
            break :blk args;
        }

        break :blk &[_]ast.Node{};
    };

    const blockArgument = blk: {
        if (self.match(Tag.leftBrace) != null) {
            if (self.match(Tag.pipe) != null) {
                parameters = self.parameterList();
                _ = self.consume(Tag.pipe, "Expect '|' after block parameters.");
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
    const empty_stmts = self.alloc().alloc(ast.Node, 0) catch @panic("Error allocating memory");
    if (self.match(Tag.rightBrace) != null) return .{ .Body = ast.Body.init(parameters, null, empty_stmts) };

    if (!self.matchLine()) {
        const expr = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        expr.* = self.expression();
        self.ignoreLine();
        _ = self.consume(Tag.rightBrace, "Expect '}' at end of block.");
        return .{ .Body = ast.Body.init(parameters, expr, null) };
    }

    const empty_stmts2 = self.alloc().alloc(ast.Node, 0) catch @panic("Error allocating memory");
    if (self.match(Tag.rightBrace) != null) return .{ .Body = ast.Body.init(parameters, null, empty_stmts2) };

    var statements: std.ArrayListUnmanaged(ast.Node) = .empty;

    while (self.peek() != Tag.eof) {
        statements.append(self.alloc(), self.definition()) catch @panic("Finish Body: appending failed");
        self.consumeLine();

        if (self.match(Tag.rightBrace) != null) break;
    }

    return .{ .Body = ast.Body.init(parameters, null, statements.toOwnedSlice(self.alloc()) catch @panic("finishBody toOwnedSlice error")) };
}

fn argumentsList(self: *Parser) []ast.Node {
    var arguments: std.ArrayListUnmanaged(ast.Node) = .empty;

    self.ignoreLine();

    while (true) {
        arguments.append(self.alloc(), self.expression()) catch @panic("Memory access limited");
        if (self.match(Tag.comma) == null) break;
        self.ignoreLine();
    }
    return arguments.toOwnedSlice(self.alloc()) catch @panic("argumentsList Panic");
}

fn parameterList(self: *Parser) []Token {
    var arguments: std.ArrayListUnmanaged(Token) = .empty;

    self.ignoreLine();

    while (true) {
        arguments.append(self.alloc(), self.consume(Tag.name, "Expect parameter name.").?) catch @panic("Memory access limited");
        if (self.match(Tag.comma) == null) break;
        self.ignoreLine();
    }
    return arguments.toOwnedSlice(self.alloc()) catch @panic("parameterList Panic");
}

fn superCall(self: *Parser) ast.Node {
    var name: ?Token = null;

    if (self.match(Tag.dot) != null) {
        name = self.consume(Tag.name, "Expect method name after 'super.'.");
    }

    const arguments = self.finishCall();
    const blockArgument = self.alloc().create(?ast.Node) catch @panic("Error allocating memory");
    blockArgument.* = arguments.blockArgument;
    return .{ .SuperExpr = ast.SuperExpr.init(name, arguments.arguments, blockArgument) };
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
    if (self.match(Tag.interpolation) != null) return self.stringInterpolation();

    self.err("Expect Expression \n");

    if (self.previous) |prev| {
        return .{ .NullExpr = ast.NullExpr.init(prev) };
    }
    _ = self.consumeToken();
    return .{ .NullExpr = ast.NullExpr.init(self.previous.?) };
}

fn stringInterpolation(self: *Parser) ast.Node {
    const firstToken = self.previous.?;

    while (true) {
        self.ignoreLine();
        _ = self.expression();
        self.ignoreLine();

        if (self.match(Tag.string) != null) {
            break;
        }
        if (self.match(Tag.interpolation) == null) {
            self.err("Expect end of string interpolation.");
            break;
        }
    }

    return .{ .StringExpr = ast.StringExpr.init(firstToken) };
}

fn grouping(self: *Parser) ast.Node {
    const leftParent = self.previous.?;
    const expr = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
    expr.* = self.expression();
    const rightParen = self.consume(Tag.rightParen, "Expect ')' after expression.");
    return ast.Node{ .GroupingExpr = ast.GroupingExpr.init(leftParent, expr, rightParen.?) };
}

fn listLiteral(self: *Parser) ast.Node {
    const leftBracket = self.previous.?;
    var elements: std.ArrayListUnmanaged(Node) = .empty;

    self.ignoreLine();
    while (self.peek() != Tag.rightBracket) {
        const expr = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        expr.* = self.expression();

        elements.append(self.alloc(), expr.*) catch @panic("Error appending to list");

        self.ignoreLine();

        if (self.match(Tag.comma) == null) break;

        self.ignoreLine();
    }

    const rightBracket = self.consume(Tag.rightBracket, "Expect ']' after list elements.");
    return ast.Node{ .ListExpr = ast.ListExpr.init(leftBracket, elements.toOwnedSlice(self.alloc()) catch @panic("Error allocating memory"), rightBracket.?) };
}

fn mapLiteral(self: *Parser) ast.Node {
    const leftBrace = self.previous.?;
    var entries: std.ArrayListUnmanaged(Node) = .empty;

    self.ignoreLine();
    while (self.peek() != Tag.rightBrace) {
        const key = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        key.* = self.expression();
        entries.append(self.alloc(), key.*) catch @panic("Error appending to list");

        self.ignoreLine();

        if (self.match(Tag.colon) == null) break;

        self.ignoreLine();

        const value = self.alloc().create(ast.Node) catch @panic("Error allocating memory");
        value.* = self.expression();
        entries.append(self.alloc(), value.*) catch @panic("Error appending to list");

        self.ignoreLine();

        if (self.match(Tag.comma) == null) break;

        self.ignoreLine();
    }

    const rightBrace = self.consume(Tag.rightBrace, "Expect '}' after map entries.");
    return ast.Node{ .MapExpr = ast.MapExpr.init(leftBrace, entries.toOwnedSlice(self.alloc()) catch @panic("Error allocating memory"), rightBrace.?) };
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

    return self.consumeToken();
}

pub fn matchAny(self: *Parser, tags: []const Tag) ?Token {
    for (tags) |tag| {
        const result = self.match(tag);
        if (result != null) return result;
    }
    return null;
}

pub fn peek(self: *Parser) Tag {
    if (self.current == null) {
        if (self.next != null) {
            self.current = self.next;
            self.next = null;
        } else {
            self.current = self.lexer.readToken() catch @panic("Unexpected error");
        }
    }
    return self.current.?.type;
}

fn peekNext(self: *Parser) Tag {
    _ = self.peek(); // Ensure current is populated
    // Read the next token after current
    const next = self.lexer.readToken() catch @panic("Unexpected error");
    // Store it back by setting current to current, and we'll use next field
    // Actually, simpler: just peek at the lexer's next token
    // We need to be careful - the lexer advances. Store the result.
    self.next = next;
    return next.type;
}

fn consumeLine(self: *Parser) void {
    if (self.peek() == Tag.eof) return;
    _ = self.consume(Tag.line, "Expect a new line.");
    self.ignoreLine();
}

fn consume(self: *Parser, tag: Tag, message: ?[]const u8) ?Token {
    const tok = self.consumeToken();
    if (tok.?.type != tag) {
        self.err(message orelse "Unexpected token.");
    }

    return tok;
}

pub fn consumeToken(self: *Parser) ?Token {
    _ = self.peek();
    self.previous = self.current;
    self.current = null;
    return self.previous;
}

pub fn parseInfix(self: *Parser, types: []const Tag, parseOperands: fn (parser: *Parser) ast.Node) ast.Node {
    var expr = parseOperands(self);

    while (self.matchAny(types) != null) {
        const operator = self.previous;
        self.ignoreLine();
        var right = parseOperands(self);
        expr = ast.Node{ .InfixExpr = ast.InfixExpr.init(&expr, operator.?, &right) };
    }

    return expr;
}

pub fn err(self: *Parser, message: []const u8) void {
    const error_token = if (self.current) |current| current else self.previous.?;

    self.errors.append(self.alloc(), .{
        .message = message,
        .token = error_token,
    }) catch @panic("Error adding to errors");

    if (self.reporter) |reporter| {
        reporter.reportError(error_token, message);
    }
}
