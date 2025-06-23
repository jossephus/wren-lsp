const std = @import("std");
pub const Chars = enum(u8) {
    tab = 0x09,
    lineFeed = 0x0a,
    carriageReturn = 0x0d,
    space = 0x20,
    bang = 0x21,
    quote = 0x22,
    percent = 0x25,
    amp = 0x26,
    leftParen = 0x28,
    rightParen = 0x29,
    star = 0x2a,
    plus = 0x2b,
    comma = 0x2c,
    minus = 0x2d,
    dot = 0x2e,
    slash = 0x2f,

    zero = 0x30,
    nine = 0x39,
    colon = 0x3a,
    less = 0x3c,
    equal = 0x3d,
    greater = 0x3e,
    question = 0x3f,
    upperA = 0x41,
    upperF = 0x46,
    upperZ = 0x5a,

    leftBracket = 0x5b,
    backslash = 0x5c,
    rightBracket = 0x5d,
    caret = 0x5e,
    underscore = 0x5f,
    lowerA = 0x61,
    lowerF = 0x66,
    lowerX = 0x78,
    lowerZ = 0x7a,
    leftBrace = 0x7b,
    pipe = 0x7c,
    rightBrace = 0x7d,
    tilde = 0x7e,

    pub fn is_alpha_numeric(c: Chars) bool {
        return std.ascii.isAlphanumeric(@intFromEnum(c));
    }

    pub fn is_alpha(c: Chars) bool {
        return std.ascii.isAlphabetic(@intFromEnum(c));
    }

    pub fn is_digit(c: Chars) bool {
        return std.ascii.isDigit(@intFromEnum(c));
    }

    pub fn is_hex_digit(c: Chars) bool {
        return std.ascii.isHex(@intFromEnum(c));
    }

    pub fn is_lower(c: Chars) bool {
        return std.ascii.isLower(@intFromEnum(c));
    }

    pub fn int(c: Chars) u8 {
        return @intFromEnum(c);
    }
};
