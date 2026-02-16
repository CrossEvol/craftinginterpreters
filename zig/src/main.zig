const std = @import("std");
const Io = std.Io;

const Chunk = @import("chunk.zig").Chunk;
const disassembleChunk = @import("debug.zig").disassembleChunk;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("vm.zig").VM;

pub fn main() !void {
    // In order to allocate memory we must construct an `Allocator` instance.
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit(); // This checks for leaks.
    const allocator = debug_allocator.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    const chunk = try allocator.create(Chunk);
    chunk.* = try Chunk.init(allocator);
    defer {
        chunk.deinit();
        allocator.destroy(chunk);
    }

    var constant: i32 = undefined;
    constant = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(OpCode.OP_CONSTANT), 123);
    try chunk.write(@intCast(constant), 123);

    constant = try chunk.addConstant(3.4);
    try chunk.write(@intFromEnum(OpCode.OP_CONSTANT), 123);
    try chunk.write(@intCast(constant), 123);

    try chunk.write(@intFromEnum(OpCode.OP_ADD), 123);

    constant = try chunk.addConstant(5.6);
    try chunk.write(@intFromEnum(OpCode.OP_CONSTANT), 123);
    try chunk.write(@intCast(constant), 123);

    try chunk.write(@intFromEnum(OpCode.OP_DIVIDE), 123);
    try chunk.write(@intFromEnum(OpCode.OP_NEGATE), 123);

    try chunk.write(@intFromEnum(OpCode.OP_RETURN), 123);

    disassembleChunk(chunk, "test chunk");
    _ = try vm.interpret(chunk);
}
