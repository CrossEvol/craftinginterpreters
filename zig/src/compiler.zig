const std = @import("std");

const Scanner = @import("scanner.zig").Scanner;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: i32 = -1;
    while (true) {
        const token = scanner.scanToken();

        if (token.line != line) {
            std.debug.print(
                "{d:4} ",
                .{@as(usize, @intCast(token.line))},
            );
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        std.debug.print("{d:2} '{s}'\n", .{ @intFromEnum(token.type), token.lexeme });

        if (token.type == .TOKEN_EOF) break;
    }
}
