const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const DEBUG_LOG_GC = @import("common.zig").DEBUG_LOG_GC;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const nil_val = Value.nil_val;
const asObj = Value.asObj;
const VM = @import("vm.zig").VM;

pub fn objType(value: Value) ObjType {
    return asObj(value).type;
}

pub fn isClass(value: Value) bool {
    return isObjType(value, .OBJ_CLASS);
}

pub fn isClosure(value: Value) bool {
    return isObjType(value, .OBJ_CLOSURE);
}

pub fn isFunction(value: Value) bool {
    return isObjType(value, .OBJ_FUNCTION);
}

pub fn isInstance(value: Value) bool {
    return isObjType(value, .OBJ_INSTANCE);
}

pub fn isNative(value: Value) bool {
    return isObjType(value, .OBJ_NATIVE);
}

pub fn isString(value: Value) bool {
    return isObjType(value, .OBJ_STRING);
}

pub fn asClass(value: Value) *ObjClass {
    return @alignCast(@fieldParentPtr("obj", asObj(value)));
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

pub fn asInstance(value: Value) *ObjInstance {
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
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
};

pub const Obj = struct {
    type: ObjType,
    is_marked: bool,
    next: ?*Obj,

    pub fn init(size: usize, @"type": ObjType, next: ?*Obj) Obj {
        const obj: Obj = .{
            .type = @"type",
            .is_marked = false,
            .next = next,
        };
        if (DEBUG_LOG_GC) {
            std.debug.print(
                "0x{x} allocate {d} for {d}\n",
                .{ @intFromPtr(&obj), size, @"type" },
            );
        }

        return obj;
    }

    /// Downcast , *Obj -> *ObjClass
    pub fn asObjClass(obj: *Obj) *ObjClass {
        return @alignCast(@fieldParentPtr("obj", obj));
    }

    /// Downcast , *Obj -> *ObjInstance
    pub fn asObjInstance(obj: *Obj) *ObjInstance {
        return @alignCast(@fieldParentPtr("obj", obj));
    }

    /// Downcast , *Obj -> *ObjClosure
    pub fn asObjClosure(obj: *Obj) *ObjClosure {
        return @alignCast(@fieldParentPtr("obj", obj));
    }

    /// Downcast , *Obj -> *ObjString
    pub fn asObjString(obj: *Obj) *ObjString {
        return @alignCast(@fieldParentPtr("obj", obj));
    }

    /// Downcast , *Obj -> *ObjFunction
    pub fn asObjFunction(obj: *Obj) *ObjFunction {
        return @alignCast(@fieldParentPtr("obj", obj));
    }

    /// Downcast , *Obj -> *ObjUpvalue
    pub fn asObjUpvalue(obj: *Obj) *ObjUpvalue {
        return @alignCast(@fieldParentPtr("obj", obj));
    }

    /// Downcast , *Obj -> *ObjNative
    pub fn asObjNative(obj: *Obj) *ObjNative {
        return @alignCast(@fieldParentPtr("obj", obj));
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: i32,
    upvalue_count: usize,
    chunk: Chunk, // owned?
    name: ?*ObjString,
    vm: *VM,

    pub fn init(vm: *VM) !ObjFunction {
        return .{
            .obj = Obj.init(@sizeOf(ObjFunction), .OBJ_FUNCTION, vm.objects),
            .arity = 0,
            .upvalue_count = 0,
            .chunk = try Chunk.init(vm),
            .name = null,
            .vm = vm,
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

    pub fn init(vm: *VM, function: NativeFn) ObjNative {
        return .{
            .obj = Obj.init(@sizeOf(ObjNative), .OBJ_NATIVE, vm.objects),
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

    pub fn init(vm: *VM, chars: []const u8, hash: u32) ObjString {
        return .{
            .obj = Obj.init(@sizeOf(ObjString), .OBJ_STRING, vm.objects),
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

    pub fn init(vm: *VM, slot: *Value) ObjUpvalue {
        return .{
            .obj = Obj.init(@sizeOf(ObjUpvalue), .OBJ_UPVALUE, vm.objects),
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

    pub fn init(vm: *VM, upvalues: []?*ObjUpvalue, function: *ObjFunction) ObjClosure {
        return .{
            .obj = Obj.init(@sizeOf(ObjClosure), .OBJ_CLOSURE, vm.objects),
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

pub const ObjClass = struct {
    obj: Obj,
    name: *ObjString,

    pub fn init(vm: *VM, name: *ObjString) ObjClass {
        return .{
            .obj = Obj.init(@sizeOf(ObjClass), .OBJ_CLASS, vm.objects),
            .name = name,
        };
    }

    /// Upcast , *ObjClass -> *Obj
    pub fn asObj(self: *ObjClass) *Obj {
        return &self.obj;
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    klass: *ObjClass,
    fields: Table, // [fields]

    pub fn init(vm: *VM, klass: *ObjClass) ObjInstance {
        return .{
            .obj = Obj.init(@sizeOf(ObjInstance), .OBJ_INSTANCE, vm.objects),
            .klass = klass,
            .fields = Table.init(vm),
        };
    }

    /// Upcast , *ObjInstance -> *Obj
    pub fn asObj(self: *ObjInstance) *Obj {
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
        .OBJ_CLASS => std.debug.print("{s}", .{asClass(value).name.chars}),
        .OBJ_CLOSURE => printFunction(asClosure(value).function),
        .OBJ_FUNCTION => printFunction(asFunction(value)),
        .OBJ_INSTANCE => std.debug.print("{s} instance", .{asInstance(value).klass.name.chars}),
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
