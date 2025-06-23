const std = @import("std");
const Token = @import("token.zig").Token;
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const SourceFile = @import("source_file.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const source_file = try SourceFile.new(allocator, "index.wren",
        \\ 1 + 2
        \\  
    );
    const lexer = try Lexer.new(allocator, source_file);

    var parser = try Parser.new(allocator, lexer);
    const module = try parser.parseModule();
    for (module.statements) |stmt| {
        _ = try stmt.toString();
    }

    //while (true) {
    //const token = try lexer.readToken();
    //std.debug.print("{any} {s} {d} \n", .{ token.type, token.source.code[token.start .. token.start + token.length], token.length });
    //if (token.type == Token.Tag.eof) break;
    //}

    //while ((try lexer.readToken()).type != Token.Tag.eof) {
    //std.debug.print("Hello Wren Aanalyzer {any} \n", .{(try lexer.readToken()).type});
    //}

    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
    //std.debug.print("Hello Wren Aanalyzer {s} \n", .{(try lexer.readToken()).source});
}
