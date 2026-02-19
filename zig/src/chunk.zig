const std = @import("std");

const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
    _,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(i32),
    constants: ValueArray,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Chunk {
        const code = try std.ArrayList(u8).initCapacity(allocator, 0);
        const lines = try std.ArrayList(i32).initCapacity(allocator, 0);
        const constants = try ValueArray.init(allocator);

        return .{
            .code = code,
            .lines = lines,
            .constants = constants,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit(self.allocator);
        self.lines.deinit(self.allocator);
        self.constants.deinit();
    }

    pub fn count(self: *Chunk) i32 {
        return @intCast(self.code.items.len);
    }

    pub fn write(self: *Chunk, byte: u8, line: i32) !void {
        try self.code.append(self.allocator, byte);
        try self.lines.append(self.allocator, line);
    }

    pub fn addConstant(self: *Chunk, value: Value) i32 {
        self.constants.write(value);
        return @intCast(self.constants.values.items.len - 1);
    }
};
