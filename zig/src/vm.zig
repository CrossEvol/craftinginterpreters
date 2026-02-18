const std = @import("std");

const asString = @import("object.zig").asString;
const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const Compiler = @import("compiler.zig").Compiler;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const isString = @import("object.zig").isString;
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;
const Value = @import("value.zig").Value;
const isNumber = Value.isNumber;
const objVal = Value.objVal;
const isNil = Value.isNil;
const asObj = Value.asObj;
const isBool = Value.isBool;
const asBool = Value.asBool;
const asNumber = Value.asNumber;
const numberVal = Value.numberVal;
const valuesEqual = Value.valuesEqual;
const boolVal = Value.boolVal;
const nil_val = Value.nil_val;

const STACK_MAX = 256;

const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

const BinaryOp = enum {
    @"+", // +
    @"-", // -
    @"*", // *
    @"/", // div
    @">", // >
    @"<", // <
};

pub const VM = struct {
    chunk: *Chunk,
    ip: usize,
    stack: std.ArrayList(Value),
    objects: ?*Obj,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !VM {
        const stack = try std.ArrayList(Value).initCapacity(allocator, STACK_MAX);

        return .{
            .chunk = undefined,
            .ip = 0,
            .stack = stack,
            .objects = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit(self.allocator);
        self.freeObjects();
    }

    fn freeObjects(self: *VM) void {
        var option_object = self.objects;
        while (option_object != null) {
            if (option_object) |object| {
                const next = object.next;
                self.freeObject(object);
                option_object = next;
            }
        }
    }

    fn freeObject(self: *VM, object: *Obj) void {
        switch (object.type) {
            .OBJ_STRING => {
                const string = asString(objVal(object));
                self.allocator.free(string.chars);
                self.allocator.destroy(string);
            },
        }
    }

    fn resetStack(self: *VM) void {
        self.stack.clearRetainingCapacity();
        self.ip = 0;
    }

    fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        const Args = @TypeOf(args);
        const args_type_info = @typeInfo(Args);

        comptime {
            const valid = switch (args_type_info) {
                .@"struct" => |s| s.is_tuple,
                .void => true, // .{}
                else => false,
            };

            if (!valid) {
                @compileError("require tuple syntax:\n, got type: " ++ @typeName(Args));
            }
        }

        std.debug.print(format ++ "\n", args);

        const instruction = self.ip - 1;
        const line = self.chunk.lines.items[instruction];
        std.debug.print("[line {d}] in script\n", .{@as(usize, @intCast(line))});

        self.resetStack();
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
        compiler.* = Compiler.init(self);
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
                .OP_NIL => try self.push(nil_val),
                .OP_TRUE => try self.push(boolVal(true)),
                .OP_FALSE => try self.push(boolVal(false)),
                .OP_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(boolVal(valuesEqual(a, b)));
                },
                .OP_GREATER => {
                    const result = try self.binaryOp(.@">");
                    if (result != .INTERPRET_OK) {
                        return result;
                    }
                },
                .OP_LESS => {
                    const result = try self.binaryOp(.@"<");
                    if (result != .INTERPRET_OK) {
                        return result;
                    }
                },
                .OP_ADD => {
                    if (isString(self.peek(0)) and isString(self.peek(1))) {
                        try self.concatenate();
                    } else if (isNumber(self.peek(0)) and isNumber(self.peek(1))) {
                        const b = asNumber(self.pop());
                        const a = asNumber(self.pop());
                        const value = numberVal(a + b);
                        try self.push(value);
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_SUBTRACT => {
                    const result = try self.binaryOp(.@"-");
                    if (result != .INTERPRET_OK) {
                        return result;
                    }
                },
                .OP_MULTIPLY => {
                    const result = try self.binaryOp(.@"*");
                    if (result != .INTERPRET_OK) {
                        return result;
                    }
                },
                .OP_DIVIDE => {
                    const result = try self.binaryOp(.@"/");
                    if (result != .INTERPRET_OK) {
                        return result;
                    }
                },
                .OP_NOT => {
                    const value = boolVal(isFalsy(self.pop()));
                    try self.push(value);
                },
                .OP_NEGATE => {
                    if (!isNumber(self.peek(0))) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    const value = numberVal(-asNumber(self.pop()));
                    try self.push(value);
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

    fn binaryOp(self: *VM, comptime op: BinaryOp) !InterpretResult {
        if (!isNumber(self.peek(0)) or !isNumber(self.peek(1))) {
            self.runtimeError("Operands must be numbers.", .{});
            return .INTERPRET_RUNTIME_ERROR;
        }

        const b = asNumber(self.pop());
        const a = asNumber(self.pop());
        const result = switch (op) {
            .@"+" => numberVal(a + b),
            .@"-" => numberVal(a - b),
            .@"*" => numberVal(a * b),
            .@"/" => numberVal(a / b),
            .@">" => boolVal(a > b),
            .@"<" => boolVal(a < b),
        };
        try self.push(result);

        return .INTERPRET_OK;
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

    fn peek(self: *VM, distance: i32) Value {
        return self.stack.items[self.stack.items.len - 1 - @as(usize, @intCast(distance))];
    }

    fn isFalsy(value: Value) bool {
        return isNil(value) or (isBool(value) and !asBool(value));
    }

    fn concatenate(self: *VM) !void {
        const b = asString(self.pop());
        const a = asString(self.pop());

        const chars = try std.mem.concat(self.allocator, u8, &.{ a.chars, b.chars });

        const result = try self.takeString(chars);
        try self.push(objVal(result.asObj()));
    }

    /// take ownership of chars
    pub fn takeString(self: *VM, chars: []const u8) !*ObjString {
        const obj_string = try self.allocator.create(ObjString);
        obj_string.* = .{
            .obj = .{
                .type = .OBJ_STRING,
                .next = self.objects,
            },
            .chars = chars,
        };
        self.objects = &obj_string.obj;

        return obj_string;
    }

    /// dupe the chars
    pub fn copyString(self: *VM, chars: []const u8) !*ObjString {
        const heap_chars = try self.allocator.dupe(u8, chars);
        const obj_string = try self.allocator.create(ObjString);
        obj_string.* = .{
            .obj = .{
                .type = .OBJ_STRING,
                .next = self.objects,
            },
            .chars = heap_chars,
        };
        self.objects = &obj_string.obj;

        return obj_string;
    }
};
