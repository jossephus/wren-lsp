const std = @import("std");
const SourceFile = @import("source_file.zig");

pub const Token = struct {
    source: *const SourceFile,
    type: Tag,
    start: usize,
    length: usize,

    pub const Tag = enum {
        leftParen,
        rightParen,
        leftBracket,
        rightBracket,
        leftBrace,
        rightBrace,
        colon,
        dot,
        dotDot,
        dotDotDot,
        comma,
        star,
        slash,
        percent,
        plus,
        minus,
        pipe,
        pipePipe,
        caret,
        amp,
        ampAmp,
        question,
        bang,
        tilde,
        equal,
        less,
        lessEqual,
        lessLess,
        greater,
        greaterEqual,
        greaterGreater,
        equalEqual,
        bangEqual,
        hash,
        hashBang,

        // Keywords.
        asKeyword,
        breakKeyword,
        classKeyword,
        constructKeyword,
        continueKeyword,
        elseKeyword,
        falseKeyword,
        forKeyword,
        foreignKeyword,
        ifKeyword,
        importKeyword,
        inKeyword,
        isKeyword,
        nullKeyword,
        returnKeyword,
        staticKeyword,
        superKeyword,
        thisKeyword,
        trueKeyword,
        varKeyword,
        whileKeyword,

        field,
        staticField,
        name,
        number,
        string,
        interpolation,
        line,
        @"error",
        eof,

        pub fn format(
            self: Tag,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            try writer.print("{s}", .{@tagName(self)});
        }
    };

    pub fn new(source: *const SourceFile, @"type": Tag, start: usize, length: usize) Token {
        return .{
            .source = source,
            .type = @"type",
            .start = start,
            .length = length,
        };
    }

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}:{d}:{d}", .{ self.type, self.start, self.length });
    }

    pub fn name(self: Token) []const u8 {
        return self.source.code[self.start .. self.start + self.length];
    }
};
