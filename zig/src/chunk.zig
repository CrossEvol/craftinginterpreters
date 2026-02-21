const std = @import("std");

const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;
const VM = @import("vm.zig").VM;

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
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
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
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_RETURN,
    OP_CLASS,
    _,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(i32),
    constants: ValueArray,
    vm: *VM,

    pub fn init(vm: *VM) !Chunk {
        const code = try std.ArrayList(u8).initCapacity(vm.allocator, 0);
        const lines = try std.ArrayList(i32).initCapacity(vm.allocator, 0);
        const constants = try ValueArray.init(vm);

        return .{
            .code = code,
            .lines = lines,
            .constants = constants,
            .vm = vm,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit(self.vm.allocator);
        self.lines.deinit(self.vm.allocator);
        self.constants.deinit();
    }

    pub fn count(self: *Chunk) i32 {
        return @intCast(self.code.items.len);
    }

    pub fn write(self: *Chunk, byte: u8, line: i32) !void {
        try self.code.append(self.vm.allocator, byte);
        try self.lines.append(self.vm.allocator, line);
    }

    pub fn addConstant(self: *Chunk, value: Value) i32 {
        self.vm.push(value);
        self.constants.write(value);
        _ = self.vm.pop();
        return @intCast(self.constants.values.items.len - 1);
    }
};
