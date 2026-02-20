const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const nil_val = Value.nil_val;
const asObj = Value.asObj;

pub fn objType(value: Value) ObjType {
    return asObj(value).type;
}

pub fn isClosure(value: Value) bool {
    return isObjType(value, .OBJ_CLOSURE);
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

pub fn asClosure(value: Value) *ObjClosure {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
}

pub fn asUpvalue(value: Value) *ObjUpvalue {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
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
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
};

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: i32,
    upvalue_count: usize,
    chunk: Chunk, // owned?
    name: ?*ObjString,

    pub fn init(allocator: std.mem.Allocator, obj: ?*Obj) !ObjFunction {
        return .{
            .obj = .{
                .type = .OBJ_FUNCTION,
                .next = obj,
            },
            .arity = 0,
            .upvalue_count = 0,
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

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn init(obj: ?*Obj, slot: *Value) ObjUpvalue {
        return .{
            .obj = .{
                .type = .OBJ_UPVALUE,
                .next = obj,
            },
            .location = slot,
            .closed = nil_val,
            .next = null,
        };
    }

    /// Upcast , *ObjUpvalue -> *Obj
    pub fn asObj(self: *ObjUpvalue) *Obj {
        return &self.obj;
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,
    upvalue_count: usize,

    pub fn init(obj: ?*Obj, upvalues: []?*ObjUpvalue, function: *ObjFunction) ObjClosure {
        return .{
            .obj = .{
                .type = .OBJ_CLOSURE,
                .next = obj,
            },
            .function = function,
            .upvalues = upvalues,
            .upvalue_count = function.upvalue_count,
        };
    }

    /// Upcast , *ObjClosure -> *Obj
    pub fn asObj(self: *ObjClosure) *Obj {
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
        .OBJ_CLOSURE => printFunction(asClosure(value).function),
        .OBJ_FUNCTION => printFunction(asFunction(value)),
        .OBJ_NATIVE => std.debug.print("<native fn>", .{}),
        .OBJ_STRING => {
            std.debug.print("{s}", .{asCString(value)});
        },
        .OBJ_UPVALUE => std.debug.print("upvalue", .{}),
    }
}

fn isObjType(value: Value, @"type": ObjType) bool {
    return switch (value) {
        .obj => |obj| obj.type == @"type",
        else => false,
    };
}
