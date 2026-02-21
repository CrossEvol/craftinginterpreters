const std = @import("std");

const asFunction = @import("object.zig").asFunction;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
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

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: i32) i32 {
    const slot = chunk.code.items[@intCast(offset + 1)];
    std.debug.print("{s:<16} {d:4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i32, chunk: *Chunk, offset: i32) i32 {
    var jump: u16 = @as(u16, chunk.code.items[@intCast(offset + 1)]) << 8;
    jump |= chunk.code.items[@intCast(offset + 2)];
    std.debug.print(
        "{s:<16} {d:4} -> {d}\n",
        .{ name, offset, offset + 3 + sign * jump },
    );
    return offset + 3;
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
        .OP_POP => return simpleInstruction("OP_POP", offset),
        .OP_GET_LOCAL => return byteInstruction("OP_GET_LOCAL", chunk, offset),
        .OP_SET_LOCAL => return byteInstruction("OP_SET_LOCAL", chunk, offset),
        .OP_GET_GLOBAL => return constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .OP_DEFINE_GLOBAL => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .OP_SET_GLOBAL => return constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .OP_GET_UPVALUE => return byteInstruction("OP_GET_UPVALUE", chunk, offset),
        .OP_SET_UPVALUE => return byteInstruction("OP_SET_UPVALUE", chunk, offset),
        .OP_GET_PROPERTY => return constantInstruction("OP_GET_PROPERTY", chunk, offset),
        .OP_SET_PROPERTY => return constantInstruction("OP_SET_PROPERTY", chunk, offset),
        .OP_EQUAL => return simpleInstruction("OP_EQUAL", offset),
        .OP_GREATER => return simpleInstruction("OP_GREATER", offset),
        .OP_LESS => return simpleInstruction("OP_LESS", offset),
        .OP_ADD => return simpleInstruction("OP_ADD", offset),
        .OP_SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        .OP_MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        .OP_DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        .OP_NOT => return simpleInstruction("OP_NOT", offset),
        .OP_NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .OP_PRINT => return simpleInstruction("OP_PRINT", offset),
        .OP_JUMP => return jumpInstruction("OP_JUMP", 1, chunk, offset),
        .OP_JUMP_IF_FALSE => return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        .OP_LOOP => return jumpInstruction("OP_LOOP", -1, chunk, offset),
        .OP_CALL => return byteInstruction("OP_CALL", chunk, offset),
        .OP_CLOSURE => {
            var offsetU: usize = @intCast(offset);
            offsetU += 1;
            const constant = chunk.code.items[@as(usize, @intCast(offsetU))];
            offsetU += 1;
            std.debug.print("{s:<16} {d:4} ", .{ "OP_CLOSURE", constant });
            printValue(chunk.constants.values.items[@intCast(constant)]);
            std.debug.print("\n", .{});

            const function = asFunction(chunk.constants.values.items[constant]);
            for (0..function.upvalue_count) |_| {
                const is_local = chunk.code.items[offsetU];
                offsetU += 1;
                const index = chunk.code.items[offsetU];
                offsetU += 1;
                std.debug.print(
                    "{d:4}      |                     {s} {d}\n",
                    .{ offsetU - 2, if (is_local > 0) "local" else "upvalue", index },
                );
            }
            return @intCast(offsetU);
        },
        .OP_CLOSE_UPVALUE => return simpleInstruction("OP_CLOSE_UPVALUE", offset),
        .OP_RETURN => return simpleInstruction("OP_RETURN", offset),
        .OP_CLASS => return constantInstruction("OP_CLASS", chunk, offset),
        else => {
            std.debug.print("Unknown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    }
}
