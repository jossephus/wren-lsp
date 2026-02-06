//! Embedded built-in Wren module sources.
//!
//! Provides embedded sources for Wren's optional built-in modules (random, meta).
//! The BuiltinResolver uses these to provide full IDE support for built-in modules.

const std = @import("std");

pub const random_source = @embedFile("builtins/random.wren");
pub const meta_source = @embedFile("builtins/meta.wren");

pub const BuiltinModule = struct {
    name: []const u8,
    source: []const u8,
};

pub const modules: []const BuiltinModule = &.{
    .{ .name = "random", .source = random_source },
    .{ .name = "meta", .source = meta_source },
};

pub fn getBuiltinSource(name: []const u8) ?[]const u8 {
    for (modules) |m| {
        if (std.mem.eql(u8, m.name, name)) {
            return m.source;
        }
    }
    return null;
}
