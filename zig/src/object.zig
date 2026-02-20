const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const asObj = Value.asObj;

pub fn objType(value: Value) ObjType {
    return asObj(value).type;
}

pub fn isFunction(value: Value) bool {
    return isObjType(value, .OBJ_FUNCTION);
}

pub fn isNative(value: Value) bool {
    return isObjType(value, .OBJ_NATIVE);
}

pub fn isString(value: Value) bool {
    return isObjType(value, .OBJ_STRING);
}

pub fn asFunction(value: Value) *ObjFunction {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
}

pub fn asNative(value: Value) *ObjNative {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
}

pub fn asString(value: Value) *ObjString {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
}

pub fn asCString(value: Value) []const u8 {
    return asString(value).chars;
}

const ObjType = enum {
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
};

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: i32,
    chunk: Chunk, // owned?
    name: ?*ObjString,

    pub fn init(allocator: std.mem.Allocator, obj: ?*Obj) !ObjFunction {
        return .{
            .obj = .{
                .type = .OBJ_FUNCTION,
                .next = obj,
            },
            .arity = 0,
            .chunk = try Chunk.init(allocator),
            .name = null,
        };
    }

    pub fn deinit(self: *ObjFunction) void {
        self.obj.next = null;
        self.chunk.deinit();
    }

    /// Upcast , *ObjFunction -> *Obj
    pub fn asObj(self: *ObjFunction) *Obj {
        return &self.obj;
    }
};

pub const NativeFn = *const fn (arg_count: usize, args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,

    pub fn init(obj: ?*Obj, function: NativeFn) ObjNative {
        return .{
            .obj = .{
                .type = .OBJ_NATIVE,
                .next = obj,
            },
            .function = function,
        };
    }

    /// Upcast , *ObjNative -> *Obj
    pub fn asObj(self: *ObjNative) *Obj {
        return &self.obj;
    }
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,
    hash: u32,

    pub fn init(obj: ?*Obj, chars: []const u8, hash: u32) ObjString {
        return .{
            .obj = .{
                .type = .OBJ_STRING,
                .next = obj,
            },
            .chars = chars,
            .hash = hash,
        };
    }

    pub fn deinit(self: *ObjString, allocator: std.mem.Allocator) void {
        self.obj.next = null;
        allocator.free(self.chars);
    }

    /// Upcast , *ObjString -> *Obj
    pub fn asObj(self: *ObjString) *Obj {
        return &self.obj;
    }
};

fn printFunction(function: *ObjFunction) void {
    if (function.name) |func_name| {
        std.debug.print("<fn {s}>", .{func_name.chars});
    } else {
        std.debug.print("<script>", .{});
    }
}

pub fn printObject(value: Value) void {
    switch (objType(value)) {
        .OBJ_FUNCTION => printFunction(asFunction(value)),
        .OBJ_NATIVE => std.debug.print("<native fn>", .{}),
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
