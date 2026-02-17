const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const Compiler = @import("compiler.zig").Compiler;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;
const Value = @import("value.zig").Value;

const STACK_MAX = 256;

const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

const BinaryOp = enum {
    add, // +
    sub, // -
    mul, // *
    div, // div
};

pub const VM = struct {
    chunk: *Chunk,
    ip: usize,
    stack: std.ArrayList(Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !VM {
        const stack = try std.ArrayList(Value).initCapacity(allocator, STACK_MAX);

        return .{
            .chunk = undefined,
            .ip = 0,
            .stack = stack,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit(self.allocator);
    }

    fn resetStack(self: *VM) void {
        self.stack.clearAndFree();
        self.ip = 0;
    }

    pub fn stackTop(self: *VM) i32 {
        return @intCast(self.stack.items.len);
    }

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        const chunk = try self.allocator.create(Chunk);
        defer {
            chunk.deinit();
            self.allocator.destroy(chunk);
        }

        chunk.* = try Chunk.init(self.allocator);

        const compiler = try self.allocator.create(Compiler);
        compiler.* = Compiler.init();
        if (!compiler.compile(source, chunk)) {
            return .INTERPRET_COMPILE_ERROR;
        }

        self.chunk = chunk;
        self.ip = 0;

        const result = try self.run();

        return result;
    }

    fn run(self: *VM) !InterpretResult {
        while (true) {
            if (common.DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (self.stack.items) |slot| {
                    std.debug.print("[", .{});
                    printValue(slot);
                    std.debug.print("]", .{});
                }
                std.debug.print("\n", .{});
                _ = disassembleInstruction(self.chunk, @intCast(self.ip));
            }

            const instruction: OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    try self.push(constant);
                },
                .OP_ADD => try self.binaryOp(.add),
                .OP_SUBTRACT => try self.binaryOp(.sub),
                .OP_MULTIPLY => try self.binaryOp(.mul),
                .OP_DIVIDE => try self.binaryOp(.div),
                .OP_NEGATE => {
                    const value = self.pop();
                    try self.push(-value);
                },
                .OP_RETURN => {
                    printValue(self.pop());
                    std.debug.print("\n", .{});
                    return .INTERPRET_OK;
                },
                else => {},
            }
        }
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readConstant(self: *VM) Value {
        return self.chunk.constants.values.items[self.readByte()];
    }

    fn binaryOp(self: *VM, comptime op: BinaryOp) !void {
        const b = self.pop();
        const a = self.pop();
        const result = switch (op) {
            .add => a + b,
            .sub => a - b,
            .mul => a * b,
            .div => a / b,
        };
        try self.push(result);
    }

    pub fn push(self: *VM, value: Value) !void {
        try self.stack.append(self.allocator, value);
    }

    pub fn pop(self: *VM) Value {
        if (self.stack.pop()) |item| {
            return item;
        }
        @panic("stack underflow");
    }
};
