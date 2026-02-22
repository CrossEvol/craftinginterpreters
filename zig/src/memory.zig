const std = @import("std");

const common = @import("common.zig");
const DEBUG_TRACE_EXECUTION = common.DEBUG_TRACE_EXECUTION;
const DEBUG_STRESS_GC = common.DEBUG_STRESS_GC;
const DEBUG_LOG_GC = common.DEBUG_LOG_GC;
const UINT8_COUNT = common.UINT8_COUNT;
const ObjectNsp = @import("object.zig");
const Obj = ObjectNsp.Obj;
const asClosure = ObjectNsp.asClosure;
const asBoundMethod = ObjectNsp.asBoundMethod;
const asClass = ObjectNsp.asClass;
const asFunction = ObjectNsp.asFunction;
const asInstance = ObjectNsp.asInstance;
const asNative = ObjectNsp.asNative;
const asString = ObjectNsp.asString;
const asUpvalue = ObjectNsp.asUpvalue;
const printValue = @import("value.zig").Value.printValue;
const Value = @import("value.zig").Value;
const objVal = Value.objVal;
const isObj = Value.isObj;
const asObj = Value.asObj;
const ValueArray = @import("value.zig").ValueArray;
const VM = @import("vm.zig").VM;

const GC_HEAP_GROW_FACTOR = 2;

pub const GC = struct {
    vm: *VM,
    allocator: std.mem.Allocator,

    pub fn init(vm: *VM) GC {
        return .{
            .vm = vm,
            .allocator = vm.allocator,
        };
    }

    pub fn deinit(self: *GC) void {
        self.freeObjects();
    }

    /// WARNING: hard to mimic the c impl
    pub fn reallocate(self: *GC, old_size: usize, new_size: usize) void {
        self.vm.bytes_allocated += new_size;
        self.vm.bytes_allocated -= old_size;
        if (DEBUG_STRESS_GC) {
            self.collectGarbage();
        }
        if (self.vm.bytes_allocated > self.vm.next_gc) {
            self.collectGarbage();
        }
    }

    pub fn markObject(self: *GC, object: *Obj) void {
        if (object.is_marked) return;

        if (DEBUG_LOG_GC) {
            std.debug.print("0x{x} mark ", .{@intFromPtr(object)});
            printValue(objVal(object));
            std.debug.print("\n", .{});
        }

        object.is_marked = true;

        self.vm.gray_stack.?.append(self.allocator, object) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            std.process.exit(1);
        };
    }

    pub fn markValue(self: *GC, value: Value) void {
        if (isObj(value)) {
            self.markObject(asObj(value));
        }
    }

    fn markArray(self: *GC, array: ValueArray) void {
        for (array.values.items) |value| {
            self.markValue(value);
        }
    }

    fn blackenObject(self: *GC, object: *Obj) void {
        if (DEBUG_LOG_GC) {
            std.debug.print("0x{x} blacken ", .{@intFromPtr(object)});
            printValue(objVal(object));
            std.debug.print("\n", .{});
        }

        switch (object.type) {
            .OBJ_BOUND_METHOD => {
                const bound = object.asObjBoundMethod();
                self.markValue(bound.receiver);
                self.markObject(bound.method.asObj());
            },
            .OBJ_CLASS => {
                const klass = object.asObjClass();
                self.markObject(klass.name.asObj());
                klass.methods.markTable();
            },
            .OBJ_CLOSURE => {
                const closure = object.asObjClosure();
                self.markObject(closure.function.asObj());
                for (0..closure.upvalue_count) |i| {
                    const option_upvalue = closure.upvalues[i];
                    if (option_upvalue) |upvalue| {
                        self.markObject(upvalue.asObj());
                    }
                }
            },
            .OBJ_FUNCTION => {
                const function = object.asObjFunction();
                if (function.name) |name| {
                    self.markObject(name.asObj());
                }
                self.markArray(function.chunk.constants);
            },
            .OBJ_INSTANCE => {
                const instance = object.asObjInstance();
                self.markObject(instance.klass.asObj());
                instance.fields.markTable();
            },
            .OBJ_UPVALUE => {
                self.markValue(object.asObjUpvalue().closed);
            },
            .OBJ_NATIVE => {},
            .OBJ_STRING => {},
        }
    }

    fn freeObject(self: *GC, object: *Obj) void {
        if (DEBUG_LOG_GC) {
            std.debug.print(
                "0x{x} free type {}\n",
                .{ @intFromPtr(object), object.type },
            );
        }
        switch (object.type) {
            .OBJ_BOUND_METHOD => {
                const bound = asBoundMethod(objVal(object));
                self.allocator.destroy(bound);
            },
            .OBJ_CLASS => {
                const klass = asClass(objVal(object));
                klass.methods.deinit();
                self.allocator.destroy(klass);
            },
            .OBJ_CLOSURE => {
                const closure = asClosure(objVal(object));
                self.allocator.free(closure.upvalues);
                self.allocator.destroy(closure);
            },
            .OBJ_FUNCTION => {
                const function = asFunction(objVal(object));
                function.deinit();
                self.allocator.destroy(function);
            },
            .OBJ_INSTANCE => {
                const instance = asInstance(objVal(object));
                instance.fields.deinit();
                self.allocator.destroy(instance);
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
            .OBJ_UPVALUE => {
                const upvalue = asUpvalue(objVal(object));
                self.allocator.destroy(upvalue);
            },
        }
    }

    fn markRoots(self: *GC) void {
        for (self.vm.stack[0..self.vm.stackTop]) |slot| {
            self.markValue(slot);
        }

        for (0..self.vm.frame_count) |i| {
            self.markObject(self.vm.frames[i].closure.asObj());
        }

        var option_upvalue = self.vm.openUpvalues;
        while (option_upvalue) |upvalue| {
            self.markObject(upvalue.asObj());
            option_upvalue = upvalue.next;
        }

        self.vm.globals.markTable();
        if (self.vm.compiler) |compiler| {
            compiler.markCompilerRoots();
        }
        if (self.vm.init_string) |init_string| {
            self.markObject(init_string.asObj());
        }
    }

    fn traceReferences(self: *GC) void {
        while (self.vm.gray_stack.?.items.len > 0) {
            const object = self.vm.gray_stack.?.pop();
            self.blackenObject(object.?);
        }
    }

    fn sweep(self: *GC) void {
        var previous: ?*Obj = null;
        var option_object = self.vm.objects;
        while (option_object) |object| {
            if (object.is_marked) {
                object.is_marked = false;
                previous = object;
                option_object = object.next;
            } else {
                const unreached = object;
                option_object = object.next;
                if (previous != null) {
                    previous.?.next = object.next;
                } else {
                    self.vm.objects = object.next;
                }

                self.freeObject(unreached);
            }
        }
    }

    pub fn collectGarbage(self: *GC) void {
        var before: usize = undefined;
        if (DEBUG_LOG_GC) {
            std.debug.print("-- gc begin\n", .{});
            before = self.vm.bytes_allocated;
        }

        self.markRoots();
        self.traceReferences();
        self.vm.strings.removeWhite();
        self.sweep();

        self.vm.next_gc = self.vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

        if (DEBUG_LOG_GC) {
            std.debug.print("-- gc end\n", .{});
            std.debug.print(
                "   collected {d} bytes (from {d} to {d}) next at {d}\n",
                .{
                    self.vm.bytes_allocated - before,
                    before,
                    self.vm.bytes_allocated,
                    self.vm.next_gc,
                },
            );
        }
    }

    pub fn freeObjects(self: *GC) void {
        var option_object = self.vm.objects;
        while (option_object) |object| {
            const option_next = object.next;
            self.freeObject(object);
            if (option_next) |next| {
                option_object = next;
            } else {
                break;
            }
        }

        self.vm.gray_stack.?.clearAndFree(self.allocator);
    }
};

pub const GcTrackingAllocator = struct {
    vm: *VM,
    backing_allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(vm: *VM, backing_allocator: std.mem.Allocator) Self {
        return Self{
            .vm = vm,
            .backing_allocator = backing_allocator,
        };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };

    fn alloc(
        ctx: *anyopaque,
        len: usize,
        ptr_align: std.mem.Alignment,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.vm.gc.reallocate(0, len);
        return self.backing_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.vm.gc.reallocate(buf.len, new_len);
        return self.backing_allocator.rawResize(buf, buf_align, new_len, ret_addr);
    }

    fn remap(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.vm.gc.reallocate(buf.len, new_len);
        return self.backing_allocator.rawRemap(buf, buf_align, new_len, ret_addr);
    }

    fn free(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        ret_addr: usize,
    ) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.vm.gc.reallocate(buf.len, 0);
        self.backing_allocator.rawFree(buf, buf_align, ret_addr);
    }
};
