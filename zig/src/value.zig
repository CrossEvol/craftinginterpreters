const std = @import("std");

const asString = @import("object.zig").asString;
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

pub const Value = union(ValueType) {
    bool: bool,
    nil: void,
    number: f64,
    obj: *Obj,

    pub inline fn isBool(value: Value) bool {
        return switch (value) {
            .bool => true,
            else => false,
        };
    }

    pub inline fn isNil(value: Value) bool {
        return switch (value) {
            .nil => true,
            else => false,
        };
    }

    pub inline fn isNumber(value: Value) bool {
        return switch (value) {
            .number => true,
            else => false,
        };
    }

    pub inline fn isObj(value: Value) bool {
        return switch (value) {
            .obj => true,
            else => false,
        };
    }

    pub inline fn asBool(value: Value) bool {
        return value.bool;
    }

    pub inline fn asNumber(value: Value) f64 {
        return value.number;
    }

    pub inline fn asObj(value: Value) *Obj {
        return value.obj;
    }

    pub fn boolVal(value: bool) Value {
        return .{
            .bool = value,
        };
    }

    inline fn nilVal() Value {
        return .{
            .nil = {},
        };
    }

    pub const nil_val = nilVal();

    pub fn numberVal(value: f64) Value {
        return .{
            .number = value,
        };
    }

    pub fn objVal(object: *Obj) Value {
        return .{
            .obj = object,
        };
    }

    pub fn valuesEqual(a: Value, b: Value) bool {
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

// void printValue(Value value);
pub fn printValue(value: Value) void {
    switch (value) {
        .bool => {
            std.debug.print("{}", .{value.asBool()});
        },
        .nil => {
            std.debug.print("nil", .{});
        },
        .number => {
            std.debug.print("{d}", .{value.asNumber()});
        },
        .obj => {
            printObject(value);
        },
    }
}
