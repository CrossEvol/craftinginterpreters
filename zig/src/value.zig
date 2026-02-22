const std = @import("std");
const Io = std.Io;

const asString = @import("object.zig").asString;
const NAN_BOXING = @import("common.zig").NAN_BOXING;
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const printObject = @import("object.zig").printObject;
const VM = @import("vm.zig").VM;

const ValueType = enum {
    bool,
    nil, // [user-types]
    number,
    obj,
};

pub const Value = if (NAN_BOXING) NanBoxedValue else TaggedUnionValue;

const NanBoxedValue = struct {
    raw: u64,

    const Self = @This();

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;

    const TAG_NIL = 1; // 01.
    const TAG_FALSE = 2; // 10.
    const TAG_TRUE = 3; // 11.

    const FALSE_VAL: u64 = QNAN | TAG_FALSE;
    const TRUE_VAL: u64 = QNAN | TAG_TRUE;
    const NIL_VAL: u64 = QNAN | TAG_NIL;
    pub const nil_val: NanBoxedValue = .{ .raw = NIL_VAL };

    pub inline fn isBool(self: Self) bool {
        return (self.raw | 1) == TRUE_VAL;
    }

    pub inline fn isNil(self: Self) bool {
        return self.raw == NIL_VAL;
    }

    pub inline fn isNumber(self: Self) bool {
        return (self.raw & QNAN) != QNAN;
    }

    pub inline fn isObj(self: Self) bool {
        return (self.raw & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub inline fn asBool(self: Self) bool {
        return self.raw == TRUE_VAL;
    }

    pub inline fn asNumber(self: Self) f64 {
        return @bitCast(self.raw);
    }

    pub inline fn asObj(self: Self) *Obj {
        const ptr_int = @as(usize, @intCast(self.raw & (~(SIGN_BIT | QNAN))));
        return @ptrFromInt(ptr_int);
    }

    pub fn boolVal(b: bool) NanBoxedValue {
        return .{ .raw = if (b) TRUE_VAL else FALSE_VAL };
    }

    inline fn nilVal() NanBoxedValue {
        return .{ .raw = NIL_VAL };
    }

    pub fn numberVal(num: f64) NanBoxedValue {
        return .{ .raw = @bitCast(num) };
    }

    pub fn objVal(obj: *Obj) NanBoxedValue {
        const ptr_int = @intFromPtr(obj);
        return .{ .raw = SIGN_BIT | QNAN | @as(u64, @intCast(ptr_int)) };
    }

    pub fn valuesEqual(a: NanBoxedValue, b: NanBoxedValue) bool {
        if (isNumber(a) and isNumber(b)) {
            return asNumber(a) == asNumber(b);
        }

        return a.raw == b.raw;
    }

    // void printValue(Value value);
    pub fn printValue(value: NanBoxedValue) void {
        var threaded: std.Io.Threaded = .init(std.heap.page_allocator, .{});
        defer threaded.deinit();
        const io = threaded.io();

        var stdout_buffer: [1024]u8 = undefined;
        var stdout_file_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
        const stdout = &stdout_file_writer.interface;

        if (isBool(value)) {
            stdout.print("{}", .{value.asBool()}) catch @panic("OUTPUT ERROR");
        } else if (isNil(value)) {
            stdout.print("nil", .{}) catch @panic("OUTPUT ERROR");
        } else if (isNumber(value)) {
            stdout.print("{d}", .{value.asNumber()}) catch @panic("OUTPUT ERROR");
        } else if (isObj(value)) {
            printObject(stdout, value);
        }
        stdout.print("\n", .{}) catch @panic("OUTPUT ERROR");
        stdout.flush() catch @panic("OUTPUT ERROR");
    }
};

const TaggedUnionValue = union(ValueType) {
    bool: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub inline fn isBool(value: TaggedUnionValue) bool {
        return switch (value) {
            .bool => true,
            else => false,
        };
    }

    pub inline fn isNil(value: TaggedUnionValue) bool {
        return switch (value) {
            .nil => true,
            else => false,
        };
    }

    pub inline fn isNumber(value: TaggedUnionValue) bool {
        return switch (value) {
            .number => true,
            else => false,
        };
    }

    pub inline fn isObj(value: TaggedUnionValue) bool {
        return switch (value) {
            .obj => true,
            else => false,
        };
    }

    pub inline fn asBool(value: TaggedUnionValue) bool {
        return value.bool;
    }

    pub inline fn asNumber(value: TaggedUnionValue) f64 {
        return value.number;
    }

    pub inline fn asObj(value: TaggedUnionValue) *Obj {
        return value.obj;
    }

    pub fn boolVal(value: bool) TaggedUnionValue {
        return .{
            .bool = value,
        };
    }

    inline fn nilVal() TaggedUnionValue {
        return .{
            .nil = {},
        };
    }

    pub const nil_val = nilVal();

    pub fn numberVal(value: f64) TaggedUnionValue {
        return .{
            .number = value,
        };
    }

    pub fn objVal(object: *Obj) TaggedUnionValue {
        return .{
            .obj = object,
        };
    }

    pub fn valuesEqual(a: TaggedUnionValue, b: TaggedUnionValue) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return false;
        }

        return switch (a) {
            .bool => a.asBool() == b.asBool(),
            .nil => true,
            .number => a.asNumber() == b.asNumber(),
            .obj => asObj(a) == asObj(b),
        };
    }

    // void printValue(Value value);
    pub fn printValue(value: TaggedUnionValue) void {
        var threaded: std.Io.Threaded = .init(std.heap.page_allocator, .{});
        defer threaded.deinit();
        const io = threaded.io();

        var stdout_buffer: [1024]u8 = undefined;
        var stdout_file_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
        const stdout = &stdout_file_writer.interface;

        switch (value) {
            .bool => {
                stdout.print("{}", .{value.asBool()}) catch @panic("OUTPUT ERROR");
            },
            .nil => {
                stdout.print("nil", .{}) catch @panic("OUTPUT ERROR");
            },
            .number => {
                stdout.print("{d}", .{value.asNumber()}) catch @panic("OUTPUT ERROR");
            },
            .obj => {
                printObject(stdout, value);
            },
        }
        stdout.print("\n", .{}) catch @panic("OUTPUT ERROR");
        stdout.flush() catch @panic("OUTPUT ERROR");
    }
};

pub const ValueArray = struct {
    values: std.ArrayList(Value),
    vm: *VM,

    // void initValueArray(ValueArray* array);
    pub fn init(vm: *VM) !ValueArray {
        const values = try std.ArrayList(Value).initCapacity(vm.allocator, 0);

        return .{
            .values = values,
            .vm = vm,
        };
    }

    // void freeValueArray(ValueArray* array);
    pub fn deinit(self: *ValueArray) void {
        self.values.deinit(self.vm.allocator);
    }

    pub fn count(self: *ValueArray) i32 {
        return @intCast(self.values.items.len);
    }

    // void writeValueArray(ValueArray* array, Value value);
    pub fn write(self: *ValueArray, value: Value) void {
        self.values.append(self.vm.allocator, value) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
    }
};
