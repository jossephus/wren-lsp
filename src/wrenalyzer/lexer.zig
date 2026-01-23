const std = @import("std");
const Token = @import("token.zig").Token;
const Tag = Token.Tag;
const Chars = @import("chars.zig").Chars;
const SourceFile = @import("source_file.zig");

pub const Lexer = @This();

pub const KeyWords = std.StaticStringMap(Tag).initComptime(.{
    .{ "break", Token.Tag.breakKeyword },
    .{ "class", Token.Tag.classKeyword },
    .{ "construct", Token.Tag.constructKeyword },
    .{ "else", Token.Tag.elseKeyword },
    .{ "false", Token.Tag.falseKeyword },
    .{ "for", Token.Tag.forKeyword },
    .{ "foreign", Token.Tag.foreignKeyword },
    .{ "if", Token.Tag.ifKeyword },
    .{ "import", Token.Tag.importKeyword },
    .{ "in", Token.Tag.inKeyword },
    .{ "is", Token.Tag.isKeyword },
    .{ "null", Token.Tag.nullKeyword },
    .{ "return", Token.Tag.returnKeyword },
    .{ "static", Token.Tag.staticKeyword },
    .{ "super", Token.Tag.superKeyword },
    .{ "this", Token.Tag.thisKeyword },
    .{ "true", Token.Tag.trueKeyword },
    .{ "var", Token.Tag.varKeyword },
    .{ "while", Token.Tag.whileKeyword },
});
allocator: std.mem.Allocator,
punctuators: std.AutoHashMap(Chars, []const Punc),
source: SourceFile,
interpolations: std.ArrayListUnmanaged(usize),
start: usize,
current: usize,

pub fn new(allocator: std.mem.Allocator, source: SourceFile) !Lexer {
    return .{
        .allocator = allocator,
        .punctuators = try initPunctuators(allocator),
        .source = source,
        .start = 0,
        .current = 0,
        .interpolations = .empty,
    };
}

fn readField(self: *Lexer) !Token {
    var tag = Tag.field;
    if (self.match(Chars.underscore)) {
        tag = Tag.staticField;
    }

    while (!self.isAtEnd() and std.ascii.isAlphanumeric(self.source.code[self.current])) {
        self.advance();
    }

    return self.makeToken(tag);
}

pub fn readToken(self: *Lexer) !Token {
    if (self.current >= self.source.code.len) return self.makeToken(Tag.eof);

    self.skipWhitespaces();

    // TODO: Should we only check this once and combine the above one? // more tests will tell
    if (self.current >= self.source.code.len) return self.makeToken(Tag.eof);

    self.start = self.current;
    const c = self.source.code[self.current];
    self.advance();

    if (self.interpolations.items.len == 0) {
        //if (c == Chars.leftParen) {
        //_interpolations[-1] = _interpolations[-1] + 1
        //} else if (c == Chars.rightParen) {
        //_interpolations[-1] = _interpolations[-1] - 1

        //// The last ")" in an interpolated expression ends the expression and
        //// resumes the string.
        //if (_interpolations[-1] == 0) {
        //// This is the final ")", so the interpolation expression has ended.
        //// This ")" now begins the next section of the template string.
        //_interpolations.removeAt(-1)
        //return readString()
        //}
        //}
    }

    const val = std.meta.intToEnum(Chars, c) catch null;

    if (val != null) {
        if (self.punctuators.contains(@as(Chars, @enumFromInt(c)))) {
            const punctuator = self.punctuators.get(@as(Chars, @enumFromInt(c))).?;
            var tag = punctuator[0].token;
            var i: usize = 1;
            while (i < punctuator.len) : (i += 1) {
                switch (punctuator[i]) {
                    .char => if (!self.match(punctuator[i].char)) break,
                    .token => @panic("Token should not be in here"),
                }

                switch (punctuator[i + 1]) {
                    .token => tag = punctuator[i + 1].token,
                    .char => @panic("Char should not be in here"),
                }
                //if (self.match(punctuator[i])) break;
                //tag = punctuator[i + 1];
                i += 2;
            }

            return self.makeToken(tag);
        }
    }

    if (c == Chars.less.int()) {
        if (self.match(Chars.less)) return self.makeToken(Tag.lessLess);
        if (self.match(Chars.equal)) return self.makeToken(Tag.lessEqual);
        return self.makeToken(Tag.less);
    }

    if (c == Chars.greater.int()) {
        if (self.match(Chars.greater)) return self.makeToken(Tag.greaterGreater);
        if (self.match(Chars.equal)) return self.makeToken(Tag.greaterEqual);
        return self.makeToken(Tag.greater);
    }

    if (c == Chars.underscore.int()) return self.readField();
    if (c == Chars.quote.int()) return self.readString();

    if (c == Chars.zero.int() and self.peek() == Chars.lowerX.int()) return self.readHexNumber();

    if (std.ascii.isDigit(c)) return self.readNumber();
    if (std.ascii.isAlphabetic(c)) return self.readName();

    return self.makeToken(Tag.@"error");
}

fn skipWhitespaces(self: *Lexer) void {
    while (true) {
        const c = self.peek();

        if (c == Chars.tab.int() or c == Chars.carriageReturn.int() or c == Chars.space.int()) {
            self.advance();
        } else if (c == Chars.slash.int() and self.peekN(1) == Chars.slash.int()) {
            while (self.peek() != Chars.lineFeed.int() and !self.isAtEnd()) {
                self.advance();
            }
        } else if (c == Chars.slash.int() and self.peekN(1) == Chars.star.int()) {
            self.advance();
            self.advance();

            var nesting: usize = 1;

            while (nesting > 0) {
                if (self.isAtEnd()) return;
                if (self.peek() == Chars.slash.int() and self.peekN(1) == Chars.star.int()) {
                    self.advance();
                    self.advance();
                    nesting += 1;
                } else if (self.peek() == Chars.star.int() and self.peekN(1) == Chars.slash.int()) {
                    self.advance();
                    self.advance();
                    nesting -= 1;
                    if (nesting == 0) break;
                } else {
                    self.advance();
                }
            }
        } else {
            break;
        }
    }
}

fn peek(self: *Lexer) i8 {
    return peekN(self, 0);
}

fn peekN(self: *Lexer, n: usize) i8 {
    if (self.current + n >= self.source.code.len) return -1;
    return @intCast(self.source.code[self.current + n]);
}

pub fn isAtEnd(self: *Lexer) bool {
    return self.current >= self.source.code.len;
}

fn readNumber(self: *Lexer) !Token {
    while (!self.isAtEnd() and std.ascii.isDigit(self.source.code[self.current])) {
        self.advance();
    }

    return self.makeToken(Tag.number);
}

fn readHexNumber(self: *Lexer) !Token {
    self.advance();
    while (!self.isAtEnd() and std.ascii.isHex(self.source.code[self.current])) {
        self.advance();
    }

    return self.makeToken(Tag.number);
}

fn readString(self: *Lexer) !Token {
    var tag = Tag.string;

    while (self.current < self.source.code.len - 1) {
        const c = self.source.code[self.current];
        self.advance();

        if (c == Chars.backslash.int()) {
            self.advance();
        } else if (c == Chars.percent.int()) {
            self.advance();
            try self.interpolations.append(self.allocator, 1);
            tag = Tag.interpolation;
            break;
        } else if (c == Chars.quote.int()) {
            break;
        }
    }
    return self.makeToken(tag);
}

fn makeToken(self: *Lexer, tag: Tag) Token {
    return Token.new(self.source, tag, self.start, self.current - self.start);
}

fn readName(self: *Lexer) !Token {
    while (!self.isAtEnd() and std.ascii.isAlphabetic(self.source.code[self.current])) {
        self.advance();
    }

    const text = self.source.code[self.start..self.current];
    var @"type" = Tag.name;

    if (KeyWords.has(text)) {
        @"type" = KeyWords.get(text).?;
    }
    return Token.new(self.source, @"type", self.start, self.current - self.start);
}

fn match(self: *Lexer, c: Chars) bool {
    if (self.isAtEnd()) return false;

    if (self.current >= self.source.code.len) return false;

    if (self.source.code[self.current] != c.int()) return false;

    self.advance();

    return true;
}

fn advance(self: *Lexer) void {
    self.current += 1;
}

const PuncTag = enum { char, token };
const Punc = union(PuncTag) {
    char: Chars,
    token: Token.Tag,
};

pub fn initPunctuators(allocator: std.mem.Allocator) !std.AutoHashMap(Chars, []const Punc) {
    var map = std.AutoHashMap(Chars, []const Punc).init(allocator);

    try map.put(Chars.leftParen, &.{Punc{ .token = Token.Tag.leftParen }});
    try map.put(Chars.rightParen, &.{Punc{ .token = Token.Tag.rightParen }});
    try map.put(Chars.leftBracket, &.{Punc{ .token = Token.Tag.leftBracket }});
    try map.put(Chars.rightBracket, &.{Punc{ .token = Token.Tag.rightBracket }});
    try map.put(Chars.leftBrace, &.{Punc{ .token = Token.Tag.leftBrace }});
    try map.put(Chars.rightBrace, &.{Punc{ .token = Token.Tag.rightBrace }});
    try map.put(Chars.colon, &.{Punc{ .token = Token.Tag.colon }});
    try map.put(Chars.comma, &.{Punc{ .token = Token.Tag.comma }});
    try map.put(Chars.star, &.{Punc{ .token = Token.Tag.star }});
    try map.put(Chars.slash, &.{Punc{ .token = Token.Tag.slash }});
    try map.put(Chars.percent, &.{Punc{ .token = Token.Tag.percent }});
    try map.put(Chars.plus, &.{Punc{ .token = Token.Tag.plus }});
    try map.put(Chars.minus, &.{Punc{ .token = Token.Tag.minus }});
    try map.put(Chars.tilde, &.{Punc{ .token = Token.Tag.tilde }});
    try map.put(Chars.caret, &.{Punc{ .token = Token.Tag.caret }});
    try map.put(Chars.question, &.{Punc{ .token = Token.Tag.question }});
    try map.put(Chars.lineFeed, &.{Punc{ .token = Token.Tag.line }});

    try map.put(Chars.pipe, &.{ .{ .token = Token.Tag.pipe }, .{ .char = Chars.pipe }, .{ .token = Token.Tag.pipePipe } });
    try map.put(Chars.amp, &.{ .{ .token = Token.Tag.amp }, .{ .char = Chars.amp }, .{ .token = Token.Tag.ampAmp } });
    try map.put(Chars.bang, &.{ .{ .token = Token.Tag.bang }, .{ .char = Chars.equal }, .{ .token = Token.Tag.bangEqual } });

    try map.put(Chars.equal, &.{
        Punc{ .token = Token.Tag.equal },
        Punc{ .char = Chars.equal },
        Punc{ .token = Token.Tag.equalEqual },
    });
    try map.put(Chars.dot, &.{
        .{ .token = Token.Tag.dot },
        .{ .char = Chars.dot },
        .{ .token = Token.Tag.dotDot },
        .{ .char = Chars.dot },
        .{ .token = Token.Tag.dotDotDot },
    });

    return map;
}

test "lexer handles hex numbers without panic" {
    const allocator = std.testing.allocator;
    const src = "static carriageReturn { 0x0d }";
    const source_file = try @import("source_file.zig").new(allocator, "test.wren", src);
    var lexer = try Lexer.new(allocator, source_file);
    defer lexer.punctuators.deinit();

    var count: usize = 0;
    while (count < 100) : (count += 1) {
        const tok = try lexer.readToken();
        if (tok.type == .eof) break;
    }
    try std.testing.expect(count < 100);
}

test "lexer handles fields without panic" {
    const allocator = std.testing.allocator;
    const src = "_field _staticField";
    const source_file = try @import("source_file.zig").new(allocator, "test.wren", src);
    var lexer = try Lexer.new(allocator, source_file);
    defer lexer.punctuators.deinit();

    const tok1 = try lexer.readToken();
    try std.testing.expectEqual(Tag.field, tok1.type);

    const tok2 = try lexer.readToken();
    try std.testing.expectEqual(Tag.staticField, tok2.type);
}

test "lexer handles source ending with identifier" {
    const allocator = std.testing.allocator;
    const src = "name";
    const source_file = try @import("source_file.zig").new(allocator, "test.wren", src);
    var lexer = try Lexer.new(allocator, source_file);
    defer lexer.punctuators.deinit();

    const tok1 = try lexer.readToken();
    try std.testing.expectEqual(Tag.name, tok1.type);

    const tok2 = try lexer.readToken();
    try std.testing.expectEqual(Tag.eof, tok2.type);
}

test "lexer handles source ending with number" {
    const allocator = std.testing.allocator;
    const src = "123";
    const source_file = try @import("source_file.zig").new(allocator, "test.wren", src);
    var lexer = try Lexer.new(allocator, source_file);
    defer lexer.punctuators.deinit();

    const tok1 = try lexer.readToken();
    try std.testing.expectEqual(Tag.number, tok1.type);

    const tok2 = try lexer.readToken();
    try std.testing.expectEqual(Tag.eof, tok2.type);
}

test "lexer handles source ending with hex number" {
    const allocator = std.testing.allocator;
    const src = "0xFF";
    const source_file = try @import("source_file.zig").new(allocator, "test.wren", src);
    var lexer = try Lexer.new(allocator, source_file);
    defer lexer.punctuators.deinit();

    const tok1 = try lexer.readToken();
    try std.testing.expectEqual(Tag.number, tok1.type);

    const tok2 = try lexer.readToken();
    try std.testing.expectEqual(Tag.eof, tok2.type);
}
