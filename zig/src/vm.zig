const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const DEBUG_TRACE_EXECUTION = common.DEBUG_TRACE_EXECUTION;
const DEBUG_STRESS_GC = common.DEBUG_STRESS_GC;
const DEBUG_LOG_GC = common.DEBUG_LOG_GC;
const UINT8_COUNT = common.UINT8_COUNT;
const Compiler = @import("compiler.zig").Compiler;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const GC = @import("memory.zig").GC;
const ObjectNsp = @import("object.zig");
const asString = ObjectNsp.asString;
const isString = ObjectNsp.isString;
const Obj = ObjectNsp.Obj;
const ObjFunction = ObjectNsp.ObjFunction;
const isInstance = ObjectNsp.isInstance;
const asInstance = ObjectNsp.asInstance;
const ObjClosure = ObjectNsp.ObjClosure;
const ObjUpvalue = ObjectNsp.ObjUpvalue;
const objType = ObjectNsp.objType;
const asFunction = ObjectNsp.asFunction;
const asUpvalue = ObjectNsp.asUpvalue;
const asClass = ObjectNsp.asClass;
const asClosure = ObjectNsp.asClosure;
const ObjNative = ObjectNsp.ObjNative;
const ObjInstance = ObjectNsp.ObjInstance;
const ObjClass = ObjectNsp.ObjClass;
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
const ValueArray = @import("value.zig").ValueArray;

const GC_HEAP_GROW_FACTOR = 2;
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
    closure: *ObjClosure, // managed by vm.objects
    ip: usize,
    start: usize,
    slots: []Value, // span

    pub fn init(closure: *ObjClosure, ip: usize, start: usize, slots: []Value) CallFrame {
        return .{
            .closure = closure,
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
    openUpvalues: ?*ObjUpvalue,

    bytes_allocated: usize,
    next_gc: usize,
    objects: ?*Obj,
    gray_stack: std.ArrayList(*Obj),

    compiler: ?*Compiler,

    gc: GC,
    allocator: std.mem.Allocator,

    pub fn create(allocator: std.mem.Allocator) !*VM {
        const vm = try allocator.create(VM);

        const gray_stack = std.ArrayList(*Obj).initCapacity(allocator, 0) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            std.process.exit(1);
        };

        vm.* = .{
            .frames = undefined,
            .frame_count = 0,
            .stack = undefined,
            .stackTop = 0,
            .globals = undefined,
            .strings = undefined,
            .openUpvalues = null,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
            .objects = null,
            .gray_stack = gray_stack,
            .compiler = null,
            .gc = undefined,
            .allocator = allocator,
        };

        vm.resetStack();
        vm.objects = null;
        vm.globals = Table.init(vm);
        vm.strings = Table.init(vm);
        vm.gc = GC.init(vm);
        vm.defineNative("clock", clockNative);

        return vm;
    }

    pub fn destroy(self: *VM) void {
        self.globals.deinit();
        self.strings.deinit();
        self.gc.deinit();
        self.allocator.destroy(self);
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
        self.openUpvalues = null;
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
            const function = frame.closure.function;
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
        var compiler = Compiler.init(self);
        self.compiler = &compiler;

        const option_function = compiler.compile(source);

        if (option_function) |function| {
            self.push(objVal(function.asObj()));
            const closure = self.newClosure(function);
            _ = self.pop();
            self.push(objVal(closure.asObj()));
            _ = self.call(closure, 0);

            return try self.run();
        } else {
            return .INTERPRET_COMPILE_ERROR;
        }
    }

    fn run(self: *VM) !InterpretResult {
        var frame = &self.frames[self.frame_count - 1];
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (self.stack[0..self.stackTop]) |slot| {
                    std.debug.print("[", .{});
                    printValue(slot);
                    std.debug.print("]", .{});
                }
                std.debug.print("\n", .{});
                _ = disassembleInstruction(&frame.closure.function.chunk, @intCast(frame.ip));
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
                .OP_GET_UPVALUE => {
                    const slot = self.readByte();
                    self.push(frame.closure.upvalues[slot].?.location.*); // [slot]
                },
                .OP_SET_UPVALUE => {
                    const slot = self.readByte();
                    frame.closure.upvalues[slot].?.location.* = self.peek(0);
                },
                .OP_GET_PROPERTY => {
                    if (!isInstance(self.peek(0))) {
                        self.runtimeError("Only instances have properties.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }

                    const instance = asInstance(self.peek(0));
                    const name = self.readString();

                    const value, const ok = instance.fields.get(name);
                    if (!ok) {
                        self.runtimeError("Undefined property '{s}'.", .{name.chars});
                        return .INTERPRET_RUNTIME_ERROR;
                    }

                    _ = self.pop(); // Instance.
                    self.push(value);
                },
                .OP_SET_PROPERTY => {
                    if (!isInstance(self.peek(1))) {
                        self.runtimeError("Only instances have fields.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }

                    const instance = asInstance(self.peek(1));
                    _ = instance.fields.set(self.readString(), self.peek(0));
                    const value = self.pop();
                    _ = self.pop();
                    self.push(value);
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
                .OP_CLOSURE => {
                    const function = asFunction(self.readConstant());
                    const closure = self.newClosure(function);
                    self.push(objVal(closure.asObj()));
                    for (0..closure.function.upvalue_count) |i| {
                        const is_local = self.readByte();
                        const index = self.readByte();
                        if (is_local > 0) {
                            closure.upvalues[i] = self.captureUpvalue(&frame.slots[index]);
                        } else {
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
                },
                .OP_CLOSE_UPVALUE => {
                    self.closeUpvalue(&self.stack[self.stackTop - 1]);
                    _ = self.pop();
                },
                .OP_RETURN => {
                    const result = self.pop();
                    self.closeUpvalue(&frame.slots[0]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return .INTERPRET_OK;
                    }

                    self.stackTop = frame.start;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                .OP_CLASS => {
                    const klass = self.newClass(self.readString());
                    self.push(objVal(klass.asObj()));
                },
                else => {},
            }
        }
    }

    fn readByte(self: *VM) u8 {
        const frame = &self.frames[self.frame_count - 1];
        const byte = frame.closure.function.chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readShort(self: *VM) u16 {
        const frame = &self.frames[self.frame_count - 1];
        frame.ip += 2;
        const msb = frame.closure.function.chunk.code.items[frame.ip - 2];
        const lsb = frame.closure.function.chunk.code.items[frame.ip - 1];
        return @intCast(@as(u16, msb) << 8 | lsb);
    }

    fn readConstant(self: *VM) Value {
        const frame = &self.frames[self.frame_count - 1];
        return frame.closure.function.chunk.constants.values.items[self.readByte()];
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

    fn call(self: *VM, closure: *ObjClosure, arg_count: usize) bool {
        if (arg_count != closure.function.arity) {
            self.runtimeError(
                "Expected {d} arguments but got {d}.",
                .{ closure.function.arity, arg_count },
            );
            return false;
        }

        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }

        const start = self.stackTop - arg_count - 1;
        const slots = self.stack[start..];
        self.frames[self.frame_count] = CallFrame.init(closure, 0, start, slots);
        self.frame_count += 1;
        return true;
    }

    fn callValue(self: *VM, callee: Value, arg_count: usize) bool {
        if (isObj(callee)) {
            switch (objType(callee)) {
                .OBJ_CLASS => {
                    const klass = asClass(callee);
                    self.stack[self.stackTop - arg_count - 1] = objVal(self.newInstance(klass).asObj());
                    return true;
                },
                .OBJ_CLOSURE => { // [switch]
                    return self.call(asClosure(callee), arg_count);
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

    fn captureUpvalue(self: *VM, local: *Value) *ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.openUpvalues;
        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and @intFromPtr(upvalue.?.location) == @intFromPtr(local)) {
            return upvalue.?;
        }

        self.push(local.*);
        const created_upvalue = self.newUpvalue(local);
        created_upvalue.next = upvalue;
        _ = self.pop();

        if (prev_upvalue == null) {
            self.openUpvalues = created_upvalue;
        } else {
            prev_upvalue.?.next = created_upvalue;
        }

        return created_upvalue;
    }

    fn closeUpvalue(self: *VM, last: *Value) void {
        while (self.openUpvalues != null and @intFromPtr(self.openUpvalues.?.location) >= @intFromPtr(last)) {
            if (self.openUpvalues) |openUpvalues| {
                const upvalue = openUpvalues;
                upvalue.closed = upvalue.location.*;
                upvalue.location = &upvalue.closed;
                self.openUpvalues = upvalue.next;
            }
        }
    }

    fn isFalsy(value: Value) bool {
        return isNil(value) or (isBool(value) and !asBool(value));
    }

    fn concatenate(self: *VM) !void {
        const b = asString(self.peek(0));
        const a = asString(self.peek(1));

        const chars = try std.mem.concat(self.allocator, u8, &.{ a.chars, b.chars });

        const result = try self.takeString(chars);
        _ = self.pop();
        _ = self.pop();
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

    pub fn newClass(self: *VM, name: *ObjString) *ObjClass {
        self.gc.reallocate(0, @sizeOf(ObjClass));

        const klass = self.allocator.create(ObjClass) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        klass.* = ObjClass.init(self, name);
        self.objects = &klass.obj;

        return klass;
    }

    pub fn newClosure(self: *VM, function: *ObjFunction) *ObjClosure {
        self.gc.reallocate(0, function.upvalue_count * (@sizeOf(?*ObjUpvalue)));
        var upvalues = self.allocator.alloc(?*ObjUpvalue, function.upvalue_count) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        for (0..function.upvalue_count) |i| {
            upvalues[i] = null;
        }

        self.gc.reallocate(0, @sizeOf(ObjClosure));
        const closure = self.allocator.create(ObjClosure) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        closure.* = ObjClosure.init(self, upvalues, function);
        self.objects = &closure.obj;

        return closure;
    }

    pub fn newUpvalue(self: *VM, slot: *Value) *ObjUpvalue {
        self.gc.reallocate(0, @sizeOf(ObjUpvalue));

        const upvalue = self.allocator.create(ObjUpvalue) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        upvalue.* = ObjUpvalue.init(self, slot);
        self.objects = &upvalue.obj;

        return upvalue;
    }

    pub fn newFunction(self: *VM) *ObjFunction {
        self.gc.reallocate(0, @sizeOf(ObjFunction));

        const function = self.allocator.create(ObjFunction) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        function.* = ObjFunction.init(self) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        self.objects = &function.obj;

        return function;
    }

    pub fn newInstance(self: *VM, klass: *ObjClass) *ObjInstance {
        self.gc.reallocate(0, @sizeOf(ObjInstance));

        const instance = self.allocator.create(ObjInstance) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        instance.* = ObjInstance.init(self, klass);
        self.objects = &instance.obj;

        return instance;
    }

    fn newNative(self: *VM, function: NativeFn) *ObjNative {
        self.gc.reallocate(0, @sizeOf(ObjNative));

        const native = self.allocator.create(ObjNative) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        native.* = ObjNative.init(self, function);
        self.objects = &native.obj;

        return native;
    }

    fn allocateString(self: *VM, chars: []const u8, hash: u32) *ObjString {
        self.gc.reallocate(0, @sizeOf(ObjString));

        const string = self.allocator.create(ObjString) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
        string.* = ObjString.init(self, chars, hash);
        self.objects = &string.obj;

        self.push(objVal(string.asObj()));
        _ = self.strings.set(string, nil_val);
        _ = self.pop();

        return string;
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

        self.gc.reallocate(0, chars.len * @sizeOf(u8));
        const heap_chars = self.allocator.dupe(u8, chars) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
        return self.allocateString(heap_chars, hash);
    }
};
