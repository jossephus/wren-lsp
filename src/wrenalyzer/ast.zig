const std = @import("std");
const Token = @import("token.zig").Token;

const NodeTag = enum { Module, AssignmentExpr, InfixExpr, PrefixExpr, NumExpr, NullExpr, GroupingExpr, ListExpr, MapExpr, BoolExpr, ThisExpr, FieldExpr, StaticFieldExpr, StringExpr, CallExpr, Body, SuperExpr, ClassStmt, Method, ImportStmt, VarStmt, BreakStmt, IfStmt, ForStmt, WhileStmt, ReturnStmt, BlockStmt };

pub const Node = union(NodeTag) {
    Module: *Module,
    AssignmentExpr: AssignmentExpr,
    InfixExpr: InfixExpr,
    PrefixExpr: PrefixExpr,
    NumExpr: NumExpr,
    NullExpr: NullExpr,
    GroupingExpr: GroupingExpr,
    ListExpr: ListExpr,
    MapExpr: MapExpr,
    BoolExpr: BoolExpr,
    ThisExpr: ThisExpr,
    FieldExpr: FieldExpr,
    StaticFieldExpr: StaticFieldExpr,
    StringExpr: StringExpr,
    CallExpr: CallExpr,
    Body: Body,
    SuperExpr: SuperExpr,
    ClassStmt: ClassStmt,
    Method: Method,
    ImportStmt: ImportStmt,
    VarStmt: VarStmt,
    BreakStmt: BreakStmt,
    IfStmt: IfStmt,
    ForStmt: ForStmt,
    WhileStmt: WhileStmt,
    ReturnStmt: ReturnStmt,
    BlockStmt: BlockStmt,

    pub fn toString(self: Node) !void {
        var buf: [4096]u8 = undefined;

        const result = switch (self) {
            .NumExpr => |num| try num.toString(&buf),
            .InfixExpr => |infix| try infix.toString(&buf),
            .PrefixExpr => |prefix| try prefix.toString(&buf),
            .GroupingExpr => |grouping| try grouping.toString(&buf),
            .ListExpr => |list| try list.toString(&buf),
            .MapExpr => |map| try map.toString(&buf),
            .BoolExpr => |boolean| try boolean.toString(&buf),
            .NullExpr => |nul| try nul.toString(&buf),
            .ThisExpr => |this| try this.toString(&buf),
            .FieldExpr => |field| try field.toString(&buf),
            .StaticFieldExpr => |field| try field.toString(&buf),
            .StringExpr => |string| try string.toString(&buf),
            .CallExpr => |call| try call.toString(&buf),
            .SuperExpr => |super| try super.toString(&buf),
            .ClassStmt => |class| try class.toString(&buf),
            .ImportStmt => |import| try import.toString(&buf),
            .VarStmt => |v| try v.toString(&buf),
            .BreakStmt => |break_| try break_.toString(&buf),
            .IfStmt => |_if| try _if.toString(&buf),
            .ForStmt => |_for| try _for.toString(&buf),
            .WhileStmt => |_while| try _while.toString(&buf),
            .ReturnStmt => |_return| try _return.toString(&buf),
            .BlockStmt => |_block| try _block.toString(&buf),
            else => {
                std.debug.print("toString for {s} not implemented\n", .{@tagName(self)});
                @panic("Quitting");
            },
        };
        std.debug.print("{s}\n", .{result});
    }
};

pub const Module = struct {
    statements: []Node,

    pub fn init(statements: []Node) Module {
        return .{ .statements = statements };
    }

    pub fn toString(self: *Module, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Module({any})", .{self.statements});
        return fbs.getWritten();
    }
};

pub const MapEntryNode = struct {
    key: Node,
    value: Node,

    pub fn init(key: Node, value: Node) MapEntryNode {
        return .{ .key = key, .value = value };
    }

    pub fn toString(self: *MapEntryNode, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("{any}: {any}", .{ self.key, self.value });
        return fbs.getWritten();
    }
};

pub const Body = struct {
    parameters: []Token,
    expression: ?*Node,
    statements: ?[]Node,

    pub fn init(parameters: []Token, expression: ?*Node, statements: ?[]Node) Body {
        return .{ .parameters = parameters, .expression = expression, .statements = statements };
    }

    pub fn toString(self: *Body, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Body({any}: {any} {any})", .{ self.parameters, self.expression, self.statements });
        return fbs.getWritten();
    }
};

pub const Method = struct {
    foreignKeyword: ?Token,
    staticKeyword: ?Token,
    constructKeyword: ?Token,
    name: ?Token,
    parameters: []Token,
    body: *?Node,

    pub fn init(foreign: ?Token, static: ?Token, construct: ?Token, name: ?Token, parameters: []Token, body: *?Node) Method {
        return .{ .foreignKeyword = foreign, .staticKeyword = static, .constructKeyword = construct, .name = name, .parameters = parameters, .body = body };
    }

    pub fn toString(self: ListExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("{any} {any} Method:{any}({any}) {any}", .{ self.foreignKeyword, self.staticKeyword, self.name, self.parameters, self.body });
        return fbs.getWritten();
    }
};

pub const ListExpr = struct {
    leftBracket: Token,
    elements: []Node,
    rightBracket: Token,

    pub fn init(lb: Token, elems: []Node, rb: Token) ListExpr {
        return .{ .leftBracket = lb, .elements = elems, .rightBracket = rb };
    }

    pub fn toString(self: ListExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("List({any} {any} {any})", .{ self.leftBracket, self.elements, self.rightBracket });
        return fbs.getWritten();
    }
};

pub const ThisExpr = struct {
    keyword: Token,

    pub fn init(keyword: Token) ThisExpr {
        return .{ .keyword = keyword };
    }

    pub fn toString(self: ThisExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("This({any})", .{self.keyword});
        return fbs.getWritten();
    }
};

pub const NullExpr = struct {
    value: Token,

    pub fn init(value: Token) NullExpr {
        return .{ .value = value };
    }

    pub fn toString(self: NullExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Null({any})", .{self.value});
        return fbs.getWritten();
    }
};

pub const StaticFieldExpr = struct {
    name: Token,

    pub fn init(name: Token) StaticFieldExpr {
        return .{ .name = name };
    }

    pub fn toString(self: StaticFieldExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("StaticField({any})", .{self.name});
        return fbs.getWritten();
    }
};

pub const FieldExpr = struct {
    name: Token,

    pub fn init(name: Token) FieldExpr {
        return .{ .name = name };
    }

    pub fn toString(self: FieldExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Field({any})", .{self.name});
        return fbs.getWritten();
    }
};

pub const CallExpr = struct {
    receiver: ?*Node,
    name: Token,
    arguments: []const Node,
    blockArgument: *?Node,

    pub fn init(receiver: ?*Node, name: Token, arguments: []const Node, blockArgument: *?Node) CallExpr {
        return .{
            .receiver = receiver,
            .name = name,
            .arguments = arguments,
            .blockArgument = blockArgument,
        };
    }

    pub fn toString(self: CallExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Call({any} {any} {any} {any})", .{
            self.receiver, self.name, self.arguments, self.blockArgument,
        });
        return fbs.getWritten();
    }
};

pub const PrefixExpr = struct {
    operator: Token,
    right: *Node,

    pub fn init(operator: Token, right: *Node) PrefixExpr {
        return .{ .operator = operator, .right = right };
    }

    pub fn toString(self: PrefixExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Prefix({any} {any})", .{ self.operator, self.right });
        return fbs.getWritten();
    }
};

pub const GroupingExpr = struct {
    leftParen: Token,
    expression: *Node,
    rightParen: Token,

    pub fn init(left: Token, expr: *Node, right: Token) GroupingExpr {
        return .{ .leftParen = left, .expression = expr, .rightParen = right };
    }

    pub fn toString(self: GroupingExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Grouping({any} {any} {any})", .{
            self.leftParen, self.expression, self.rightParen,
        });
        return fbs.getWritten();
    }
};

pub const AssignmentExpr = struct {
    target: *Node,
    equal: Token,
    value: *Node,

    pub fn init(target: *Node, equal: Token, value: *Node) AssignmentExpr {
        return .{ .target = target, .equal = equal, .value = value };
    }

    pub fn toString(self: *AssignmentExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Assignment({any} {s} {any})", .{
            self.target, self.equal, self.value,
        });
        return fbs.getWritten();
    }
};

pub const InfixExpr = struct {
    left: *Node,
    operator: Token,
    right: *Node,

    pub fn init(left: *Node, operator: Token, right: *Node) InfixExpr {
        return .{ .left = left, .operator = operator, .right = right };
    }

    pub fn toString(self: InfixExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Infix({any} {any} {any})", .{
            self.left, self.operator, self.right,
        });
        return fbs.getWritten();
    }
};

pub const MapExpr = struct {
    leftBrace: Token,
    entries: []Node,
    rightBrace: Token,

    pub fn init(lb: Token, entries: []Node, rb: Token) MapExpr {
        return .{ .leftBrace = lb, .entries = entries, .rightBrace = rb };
    }

    pub fn toString(self: MapExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Map({any} {any} {any})", .{ self.leftBrace, self.entries, self.rightBrace });
        return fbs.getWritten();
    }
};

pub const ConditionalExpr = struct {
    condition: Node,
    question: []const u8,
    thenBranch: Node,
    colon: []const u8,
    elseBranch: Node,

    pub fn init(cond: Node, q: []const u8, then_: Node, c: []const u8, else_: Node) ConditionalExpr {
        return .{
            .condition = cond,
            .question = q,
            .thenBranch = then_,
            .colon = c,
            .elseBranch = else_,
        };
    }

    pub fn toString(self: *ConditionalExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Conditional({any} {s} {any} {s} {any})", .{
            self.condition, self.question, self.thenBranch, self.colon, self.elseBranch,
        });
        return fbs.getWritten();
    }
};

pub const NumExpr = struct {
    value: Token,

    pub fn init(value: Token) NumExpr {
        return .{ .value = value };
    }

    pub fn toString(self: NumExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Num({})", .{self.value.type});
        return fbs.getWritten();
    }
};

pub const SuperExpr = struct {
    name: ?Token,

    arguments: []const Node,
    blockArgument: *?Node,

    pub fn init(name: ?Token, args: []const Node, block: *?Node) SuperExpr {
        return .{ .name = name, .arguments = args, .blockArgument = block };
    }

    pub fn toString(self: SuperExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Super({any} {any} {any})", .{
            self.name, self.arguments, self.blockArgument,
        });
        return fbs.getWritten();
    }
};

pub const StringExpr = struct {
    value: Token,

    pub fn init(value: Token) StringExpr {
        return .{ .value = value };
    }

    pub fn toString(self: StringExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("String({any})", .{self.value});
        return fbs.getWritten();
    }
};

pub const SubscriptExpr = struct {
    receiver: Node,
    leftBracket: []const u8,
    arguments: []Node,
    rightBracket: []const u8,

    pub fn init(recv: Node, lb: []const u8, args: []Node, rb: []const u8) SubscriptExpr {
        return .{
            .receiver = recv,
            .leftBracket = lb,
            .arguments = args,
            .rightBracket = rb,
        };
    }

    pub fn toString(self: *SubscriptExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Subscript({any} {s} {any} {s})", .{
            self.receiver, self.leftBracket, self.arguments, self.rightBracket,
        });
        return fbs.getWritten();
    }
};

pub const BoolExpr = struct {
    value: bool,

    pub fn init(value: bool) BoolExpr {
        return .{ .value = value };
    }

    pub fn toString(self: BoolExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Bool({})", .{self.value});
        return fbs.getWritten();
    }
};

pub const InterpolationExpr = struct {
    strings: []const []const u8,
    expressions: []Node,

    pub fn init(strings: []const []const u8, exprs: []Node) InterpolationExpr {
        return .{ .strings = strings, .expressions = exprs };
    }

    pub fn toString(self: *InterpolationExpr, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Interpolation({any} {any})", .{
            self.strings, self.expressions,
        });
        return fbs.getWritten();
    }
};

pub const ForStmt = struct {
    variable: ?Token,
    iterator: *Node,
    body: *Node,

    pub fn init(@"var": ?Token, iter: *Node, body: *Node) ForStmt {
        return .{ .variable = @"var", .iterator = iter, .body = body };
    }

    pub fn toString(self: ForStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("For({any} {any} {any})", .{
            self.variable, self.iterator, self.body,
        });
        return fbs.getWritten();
    }
};

pub const ReturnStmt = struct {
    keyword: ?Token,
    value: *?Node,

    pub fn init(keyword: ?Token, value: *?Node) ReturnStmt {
        return .{ .keyword = keyword, .value = value };
    }

    pub fn toString(self: ReturnStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Return({any} {any})", .{ self.keyword, self.value });
        return fbs.getWritten();
    }
};

pub const BlockStmt = struct {
    statements: []Node,

    pub fn init(statements: []Node) BlockStmt {
        return .{ .statements = statements };
    }

    pub fn toString(self: BlockStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Block({any})", .{self.statements});
        return fbs.getWritten();
    }
};

pub const VarStmt = struct {
    name: ?Token,
    initializer: *?Node,

    pub fn init(name: ?Token, initializer: *?Node) VarStmt {
        return .{ .name = name, .initializer = initializer };
    }

    pub fn toString(self: VarStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Var({any} {any})", .{ self.name, self.initializer });
        return fbs.getWritten();
    }
};

pub const ImportStmt = struct {
    path: ?Token,
    variables: ?[]?Token,

    pub fn init(path: ?Token, variables: ?[]?Token) ImportStmt {
        return .{ .path = path, .variables = variables };
    }

    pub fn toString(self: ImportStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Import({any} {any})", .{ self.path, self.variables });
        return fbs.getWritten();
    }
};

pub const IfStmt = struct {
    condition: *Node,
    thenBranch: *Node,
    elseBranch: ?*Node,

    pub fn init(condition: *Node, thenBranch: *Node, elseBranch: ?*Node) IfStmt {
        return .{ .condition = condition, .thenBranch = thenBranch, .elseBranch = elseBranch };
    }

    pub fn toString(self: IfStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("If({any} {any} {any})", .{ self.condition, self.thenBranch, self.elseBranch });
        return fbs.getWritten();
    }
};

pub const BreakStmt = struct {
    keyword: ?Token,

    pub fn init(keyword: ?Token) BreakStmt {
        return .{ .keyword = keyword };
    }

    pub fn toString(self: BreakStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Break({s})", .{self.keyword.?});
        return fbs.getWritten();
    }
};

pub const WhileStmt = struct {
    condition: *Node,
    body: *Node,

    pub fn init(condition: *Node, body: *Node) WhileStmt {
        return .{ .condition = condition, .body = body };
    }

    pub fn toString(self: WhileStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("While({any} {any})", .{ self.condition, self.body });
        return fbs.getWritten();
    }
};

pub const ClassStmt = struct {
    foreignKeyword: ?Token,
    name: ?Token,
    superclass: ?Token,
    methods: []Node,

    pub fn init(foreign: ?Token, name: ?Token, superclass: ?Token, methods: []Node) ClassStmt {
        return .{ .foreignKeyword = foreign, .name = name, .superclass = superclass, .methods = methods };
    }

    pub fn toString(self: ClassStmt, buf: []u8) ![]u8 {
        var fbs = std.io.fixedBufferStream(buf);
        try fbs.writer().print("Class({any} {any} {any} {any})", .{
            self.foreignKeyword,
            self.name,
            self.superclass,
            self.methods,
        });
        return fbs.getWritten();
    }
};
