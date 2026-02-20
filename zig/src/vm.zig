const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const UINT8_COUNT = common.UINT8_COUNT;
const Compiler = @import("compiler.zig").Compiler;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const ObjectNsp = @import("object.zig");
const asString = ObjectNsp.asString;
const isString = ObjectNsp.isString;
const Obj = ObjectNsp.Obj;
const ObjFunction = ObjectNsp.ObjFunction;
const objType = ObjectNsp.objType;
const asFunction = ObjectNsp.asFunction;
const ObjNative = ObjectNsp.ObjNative;
const ObjString = ObjectNsp.ObjString;
const asNative = ObjectNsp.asNative;
const NativeFn = ObjectNsp.NativeFn;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const isNumber = Value.isNumber;
const objVal = Value.objVal;
const isNil = Value.isNil;
const asObj = Value.asObj;
const isObj = Value.isObj;
const isBool = Value.isBool;
const asBool = Value.asBool;
const asNumber = Value.asNumber;
const numberVal = Value.numberVal;
const valuesEqual = Value.valuesEqual;
const boolVal = Value.boolVal;
const nil_val = Value.nil_val;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * UINT8_COUNT;

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

const CallFrame = struct {
    function: *ObjFunction, // managed by vm.objects
    ip: usize,
    start: usize,
    slots: []Value, // span

    pub fn init(function: *ObjFunction, ip: usize, start: usize, slots: []Value) CallFrame {
        return .{
            .function = function,
            .ip = ip,
            .start = start,
            .slots = slots,
        };
    }
};

pub const VM = struct {
    frames: [FRAMES_MAX]CallFrame,
    frame_count: usize,

    stack: [STACK_MAX]Value,
    stackTop: usize,
    globals: Table,
    strings: Table,
    objects: ?*Obj,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !VM {
        var vm: VM = .{
            .frames = undefined,
            .frame_count = 0,
            .stack = undefined,
            .stackTop = 0,
            .globals = undefined,
            .strings = undefined,
            .objects = null,
            .allocator = allocator,
        };

        vm.resetStack();
        vm.objects = null;
        vm.globals = Table.init(allocator);
        vm.strings = Table.init(allocator);
        vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        self.strings.deinit();
        self.freeObjects();
    }

    fn freeObjects(self: *VM) void {
        var option_object = self.objects;
        while (option_object != null) {
            if (option_object) |object| {
                const option_next = object.next;
                self.freeObject(object);
                if (option_next) |next| {
                    option_object = next;
                } else {
                    break;
                }
            }
        }
    }

    fn freeObject(self: *VM, object: *Obj) void {
        switch (object.type) {
            .OBJ_FUNCTION => {
                const function = asFunction(objVal(object));
                function.deinit();
                self.allocator.destroy(function);
            },
            .OBJ_NATIVE => {
                const native = asNative(objVal(object));
                self.allocator.destroy(native);
            },
            .OBJ_STRING => {
                const string = asString(objVal(object));
                self.allocator.free(string.chars);
                self.allocator.destroy(string);
            },
        }
    }

    fn clockNative(arg_count: usize, args: []Value) Value {
        _ = arg_count;
        _ = args;

        const now = std.time.Instant.now() catch @panic("clockTime: timestamp failed");
        const ns = now.timestamp;

        const t = @as(f64, @floatFromInt(ns)) / @as(f64, std.time.ns_per_ms) / @as(f64, 10);

        return numberVal(t);
    }

    fn resetStack(self: *VM) void {
        self.stackTop = 0;
        self.frame_count = 0;
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

        var i = self.frame_count - 1;
        while (i >= 0) {
            const frame = self.frames[i];
            const function = frame.function;
            const instruction = frame.ip - 1;
            const line = function.chunk.lines.items[instruction];
            std.debug.print("[line {d}] in ", .{@as(usize, @intCast(line))});
            if (function.name) |name| {
                std.debug.print("{s}()\n", .{name.chars});
            } else {
                std.debug.print("script\n", .{});
            }

            if (i > 0) {
                i -= 1;
            } else {
                break;
            }
        }

        self.resetStack();
    }

    fn defineNative(self: *VM, name: []const u8, function: NativeFn) void {
        self.push(objVal(self.copyString(name).asObj()));
        self.push(objVal(self.newNative(function).asObj()));
        const key = asString(self.stack[0]);
        const value = self.stack[1];
        _ = self.globals.set(key, value);
        _ = self.pop();
        _ = self.pop();
    }

    pub fn interpret(self: *VM, source: []const u8) !InterpretResult {
        const compiler = try self.allocator.create(Compiler);
        defer {
            compiler.deinit();
            self.allocator.destroy(compiler);
        }
        compiler.* = Compiler.init(self.allocator, self);

        const option_function = compiler.compile(source);

        if (option_function) |function| {
            self.push(objVal(function.asObj()));
            _ = self.call(function, 0);

            return try self.run();
        } else {
            return .INTERPRET_COMPILE_ERROR;
        }
    }

    fn run(self: *VM) !InterpretResult {
        var frame = &self.frames[self.frame_count - 1];
        while (true) {
            if (common.DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (self.stack[0..self.stackTop]) |slot| {
                    std.debug.print("[", .{});
                    printValue(slot);
                    std.debug.print("]", .{});
                }
                std.debug.print("\n", .{});
                _ = disassembleInstruction(&frame.function.chunk, @intCast(frame.ip));
            }

            const instruction: OpCode = @enumFromInt(self.readByte());
            switch (instruction) {
                .OP_CONSTANT => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .OP_NIL => self.push(nil_val),
                .OP_TRUE => self.push(boolVal(true)),
                .OP_FALSE => self.push(boolVal(false)),
                .OP_POP => _ = self.pop(),
                .OP_GET_LOCAL => {
                    const slot = self.readByte();
                    self.push(frame.slots[slot]); // [slot]
                },
                .OP_SET_LOCAL => {
                    const slot = self.readByte();
                    frame.slots[slot] = self.peek(0);
                },
                .OP_GET_GLOBAL => {
                    const name = self.readString();
                    const value, const ok = self.globals.get(name);
                    if (!ok) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    self.push(value);
                },
                .OP_DEFINE_GLOBAL => {
                    const name = self.readString();
                    const value = self.peek(0);
                    _ = self.globals.set(name, value);
                    _ = self.pop();
                },
                .OP_SET_GLOBAL => {
                    const name = self.readString();
                    const value = self.peek(0);
                    const is_new = self.globals.set(name, value);
                    if (is_new) {
                        _ = self.globals.delete(name); // [delete]
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .OP_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(boolVal(valuesEqual(a, b)));
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
                        self.push(value);
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
                    self.push(value);
                },
                .OP_NEGATE => {
                    if (!isNumber(self.peek(0))) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    const value = numberVal(-asNumber(self.pop()));
                    self.push(value);
                },
                .OP_PRINT => {
                    printValue(self.pop());
                    std.debug.print("\n", .{});
                },
                .OP_JUMP => {
                    const offset = self.readShort();
                    frame.ip += @as(usize, @intCast(offset));
                },
                .OP_JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (isFalsy(self.peek(0))) {
                        frame.ip += @as(usize, @intCast(offset));
                    }
                },
                .OP_LOOP => {
                    const offset = self.readShort();
                    frame.ip -= @as(usize, @intCast(offset));
                },
                .OP_CALL => {
                    const arg_count = self.readByte();
                    if (!self.callValue(self.peek(arg_count), arg_count)) {
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .OP_RETURN => {
                    const result = self.pop();
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return .INTERPRET_OK;
                    }

                    self.stackTop = frame.start;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                else => {},
            }
        }
    }

    fn readByte(self: *VM) u8 {
        const frame = &self.frames[self.frame_count - 1];
        const byte = frame.function.chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) u16 {
        const frame = &self.frames[self.frame_count - 1];
        frame.ip += 2;
        const msb = frame.function.chunk.code.items[frame.ip - 2];
        const lsb = frame.function.chunk.code.items[frame.ip - 1];
        return @intCast(@as(u16, msb) << 8 | lsb);
    }

    fn readConstant(self: *VM) Value {
        const frame = &self.frames[self.frame_count - 1];
        return frame.function.chunk.constants.values.items[self.readByte()];
    }

    fn readString(self: *VM) *ObjString {
        return asString(self.readConstant());
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
        self.push(result);

        return .INTERPRET_OK;
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }

    pub fn pop(self: *VM) Value {
        self.stackTop -= 1;
        const value = self.stack[self.stackTop];
        return value;
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stackTop - 1 - @as(usize, @intCast(distance))];
    }

    fn call(self: *VM, function: *ObjFunction, arg_count: usize) bool {
        if (arg_count != function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.", .{ function.arity, arg_count });
            return false;
        }

        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }

        const start = self.stackTop - arg_count - 1;
        const slots = self.stack[start..];
        self.frames[self.frame_count] = CallFrame.init(function, 0, start, slots);
        self.frame_count += 1;
        return true;
    }

    fn callValue(self: *VM, callee: Value, arg_count: usize) bool {
        if (isObj(callee)) {
            switch (objType(callee)) {
                .OBJ_FUNCTION => { // [switch]
                    return self.call(asFunction(callee), arg_count);
                },
                .OBJ_NATIVE => {
                    const native = asNative(callee);
                    const result = native.function(arg_count, self.stack[self.stackTop - arg_count ..]);
                    self.stackTop -= arg_count + 1;
                    self.push(result);
                    return true;
                },
                else => {
                    // Non-callable object type.
                },
            }
        }

        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn isFalsy(value: Value) bool {
        return isNil(value) or (isBool(value) and !asBool(value));
    }

    fn concatenate(self: *VM) !void {
        const b = asString(self.pop());
        const a = asString(self.pop());

        const chars = try std.mem.concat(self.allocator, u8, &.{ a.chars, b.chars });

        const result = try self.takeString(chars);
        self.push(objVal(result.asObj()));
    }

    fn hashString(key: []const u8) u32 {
        var hash: u32 = 2166136261;
        for (key) |bit| {
            hash ^= bit;
            hash *%= 16777619;
        }
        return hash;
    }

    pub fn newFunction(self: *VM) *ObjFunction {
        const obj_function = self.allocator.create(ObjFunction) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        obj_function.* = ObjFunction.init(self.allocator, self.objects) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        self.objects = &obj_function.obj;

        return obj_function;
    }

    fn newNative(self: *VM, function: NativeFn) *ObjNative {
        const obj_native = self.allocator.create(ObjNative) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        obj_native.* = ObjNative.init(self.objects, function);
        self.objects = &obj_native.obj;

        return obj_native;
    }

    fn allocateString(self: *VM, chars: []const u8, hash: u32) *ObjString {
        const obj_string = self.allocator.create(ObjString) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        obj_string.* = ObjString.init(self.objects, chars, hash);
        self.objects = &obj_string.obj;
        _ = self.strings.set(obj_string, nil_val);
        return obj_string;
    }

    /// take ownership of chars
    pub fn takeString(self: *VM, chars: []const u8) !*ObjString {
        const hash = hashString(chars);
        const option_interned = self.strings.findString(chars, hash);

        if (option_interned) |interned| {
            self.allocator.free(chars);
            return interned;
        }

        return self.allocateString(chars, hash);
    }

    /// dupe the chars
    pub fn copyString(self: *VM, chars: []const u8) *ObjString {
        const hash = hashString(chars);
        const option_interned = self.strings.findString(chars, hash);

        if (option_interned) |interned| {
            return interned;
        }

        const heap_chars = self.allocator.dupe(u8, chars) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
        return self.allocateString(heap_chars, hash);
    }
};
