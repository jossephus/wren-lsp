const std = @import("std");
const wrenalyzer = @import("wrenalyzer");
const Token = wrenalyzer.Token;
const Lexer = wrenalyzer.Lexer;
const Parser = wrenalyzer.Parser;
const SourceFile = wrenalyzer.SourceFile;

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var source_file = try SourceFile.new(allocator, "index.wren",
        \\ 1 +
        \\  
    );
    const lexer = try Lexer.new(allocator, &source_file);

    var parser = try Parser.new(allocator, lexer);
    const module = try parser.parseModule();
    for (module.statements) |stmt| {
        _ = try stmt.toString();
    }

    for (parser.errors.items) |err| {
        std.debug.print("{s}", .{err.message});
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
