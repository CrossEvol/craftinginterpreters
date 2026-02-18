const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;

pub fn disassembleChunk(chunk: *Chunk, comptime name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: i32 = 0;
    while (offset < chunk.count()) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: i32) i32 {
    const constant = chunk.code.items[@as(usize, @intCast(offset + 1))];

    std.debug.print("{s:<16} {d:4} '", .{ name, constant });
    printValue(chunk.constants.values.items[@as(usize, @intCast(constant))]);
    std.debug.print("'\n", .{});

    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: i32) i32 {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

pub fn disassembleInstruction(chunk: *Chunk, offset: i32) i32 {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and
        chunk.lines.items[@intCast(offset - 1)] == chunk.lines.items[@intCast(offset)])
    {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:0>4} ", .{chunk.lines.items[@intCast(offset)]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code.items[@intCast(offset)]);

    switch (instruction) {
        .OP_CONSTANT => return constantInstruction("OP_CONSTANT", chunk, offset),
        .OP_NIL => return simpleInstruction("OP_NIL", offset),
        .OP_TRUE => return simpleInstruction("OP_TRUE", offset),
        .OP_FALSE => return simpleInstruction("OP_FALSE", offset),
        .OP_EQUAL => return simpleInstruction("OP_EQUAL", offset),
        .OP_GREATER => return simpleInstruction("OP_GREATER", offset),
        .OP_LESS => return simpleInstruction("OP_LESS", offset),
        .OP_ADD => return simpleInstruction("OP_ADD", offset),
        .OP_SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        .OP_MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        .OP_DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        .OP_NOT => return simpleInstruction("OP_NOT", offset),
        .OP_NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .OP_RETURN => return simpleInstruction("OP_RETURN", offset),
        else => {
            std.debug.print("Unknown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    }
}
