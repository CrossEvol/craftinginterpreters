const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const Compiler = @import("compiler.zig").Compiler;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const ObjectNsp = @import("object.zig");
const asString = ObjectNsp.asString;
const isString = ObjectNsp.isString;
const Obj = ObjectNsp.Obj;
const ObjString = ObjectNsp.ObjString;
const OpCode = @import("chunk.zig").OpCode;
const printValue = @import("value.zig").printValue;
const Table = @import("table.zig").Table;
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
    globals: Table,
    strings: Table,
    objects: ?*Obj,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !VM {
        const stack = try std.ArrayList(Value).initCapacity(allocator, STACK_MAX);

        return .{
            .chunk = undefined,
            .ip = 0,
            .stack = stack,
            .globals = Table.init(allocator),
            .strings = Table.init(allocator),
            .objects = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.globals.deinit();
        self.strings.deinit();
        self.stack.deinit(self.allocator);
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
        defer self.allocator.destroy(compiler);
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
                    self.push(constant);
                },
                .OP_NIL => self.push(nil_val),
                .OP_TRUE => self.push(boolVal(true)),
                .OP_FALSE => self.push(boolVal(false)),
                .OP_POP => _ = self.pop(),
                .OP_GET_LOCAL => {
                    const slot = self.readByte();
                    self.push(self.stack.items[slot]); // [slot]
                },
                .OP_SET_LOCAL => {
                    const slot = self.readByte();
                    self.stack.items[slot] = self.peek(0);
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
                    self.ip += @as(usize, @intCast(offset));
                },
                .OP_JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (isFalsy(self.peek(0))) {
                        self.ip += @as(usize, @intCast(offset));
                    }
                },
                .OP_LOOP => {
                    const offset = self.readShort();
                    self.ip -= @as(usize, @intCast(offset));
                },
                .OP_RETURN => {
                    // Exit interpreter.
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

    fn readShort(self: *VM) u16 {
        self.ip += 2;
        const msb = self.chunk.code.items[self.ip - 2];
        const lsb = self.chunk.code.items[self.ip - 1];
        return @intCast(@as(u16, msb) << 8 | lsb);
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
        self.stack.append(self.allocator, value) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
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
