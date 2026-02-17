const std = @import("std");

pub const Value = f64;

pub const ValueArray = struct {
    values: std.ArrayList(Value),
    allocator: std.mem.Allocator,

    // void initValueArray(ValueArray* array);
    pub fn init(allocator: std.mem.Allocator) !ValueArray {
        const values = try std.ArrayList(Value).initCapacity(allocator, 0);

        return .{
            .values = values,
            .allocator = allocator,
        };
    }

    // void freeValueArray(ValueArray* array);
    pub fn deinit(self: *ValueArray) void {
        self.values.deinit(self.allocator);
    }

    pub fn count(self: *ValueArray) i32 {
        return @intCast(self.values.items.len);
    }

    // void writeValueArray(ValueArray* array, Value value);
    pub fn write(self: *ValueArray, value: Value) void {
        self.values.append(self.allocator, value) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic("OOM");
        };
    }
};

// void printValue(Value value);
pub fn printValue(value: Value) void {
    std.debug.print("{d}", .{value});
}
