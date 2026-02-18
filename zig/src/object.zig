const std = @import("std");

const Value = @import("value.zig").Value;
const asObj = Value.asObj;

pub fn objType(value: Value) ObjType {
    return asObj(value).type;
}

pub fn isString(value: Value) bool {
    return isObjType(value, .OBJ_STRING);
}

pub fn asString(value: Value) *ObjString {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
}

pub fn asCString(value: Value) []const u8 {
    return asString(value).chars;
}

const ObjType = enum {
    OBJ_STRING,
};

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,

    /// Upcast , *ObjString -> *Obj
    pub fn asObj(self: *ObjString) *Obj {
        return &self.obj;
    }
};

pub fn printObject(value: Value) void {
    switch (objType(value)) {
        .OBJ_STRING => {
            std.debug.print("{s}", .{asCString(value)});
        },
    }
}

fn isObjType(value: Value, @"type": ObjType) bool {
    return switch (value) {
        .obj => |obj| obj.type == @"type",
        else => false,
    };
}
