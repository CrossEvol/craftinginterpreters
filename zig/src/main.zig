const std = @import("std");
const Io = std.Io;

const Chunk = @import("chunk.zig").Chunk;
const disassembleChunk = @import("debug.zig").disassembleChunk;
const GcTrackingAllocator = @import("memory.zig").GcTrackingAllocator;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("vm.zig").VM;

fn repl(vm: *VM) !void {
    var threaded: std.Io.Threaded = .init(vm.allocator, .{});
    defer threaded.deinit();
    const io = threaded.io();

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_file_reader: Io.File.Reader = .init(.stdin(), io, &stdin_buffer);
    const stdin = &stdin_file_reader.interface;

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
    const stdout = &stdout_file_writer.interface;

    while (true) {
        try stdout.writeAll("> ");
        try stdout.flush();

        const line = stdin.takeDelimiterInclusive('\n') catch |err| {
            if (err == error.EndOfStream) break;
            std.debug.print("\n{s}\n", .{@errorName(err)});
            break;
        };
        // should not call allocator.free(line) for it is managed by stdin.buffer

        _ = try vm.interpret(line);
    }
}

fn readFile(vm: *VM, path: []const u8) []const u8 {
    var threaded: std.Io.Threaded = .init(vm.allocator, .{});
    defer threaded.deinit();
    const io = threaded.io();

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
    const stdout = &stdout_file_writer.interface;

    stdout.flush() catch |err| {
        std.debug.print("Could not flush stdout: {s}\n", .{@errorName(err)});
    };

    const cwd = std.Io.Dir.cwd();
    const file = cwd.openFile(
        io,
        path,
        .{ .mode = .read_only },
    ) catch |err| {
        std.debug.print("Could not open file \"{s}: {s}\".\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };
    defer file.close(io);

    const stat = file.stat(io) catch |err| {
        std.debug.print("Could not stat file \"{s}: {s}\".\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };
    const file_size = stat.size;
    const data = vm.allocator.alloc(u8, file_size) catch |err| {
        std.debug.print("Not enough memory to read \"{s}\": {s}.\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };

    var fr = file.reader(io, data);
    var reader = &fr.interface;

    reader.readSliceAll(data) catch |err| {
        std.debug.print("Could not read file \"{s}: {s}\".\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };

    return data;
}

fn runFile(vm: *VM, path: []const u8) !void {
    const source = readFile(vm, path);
    defer vm.allocator.free(source);

    const result = try vm.interpret(source);
    if (result == .INTERPRET_COMPILE_ERROR) std.process.exit(65);
    if (result == .INTERPRET_RUNTIME_ERROR) std.process.exit(70);
}

pub fn main() !void {
    // In order to allocate memory we must construct an `Allocator` instance.
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit(); // This checks for leaks.
    const gpa = debug_allocator.allocator();

    // 1. Allocate the structures first
    const gc_tracker = try gpa.create(GcTrackingAllocator);
    defer gpa.destroy(gc_tracker);

    const vm = try gpa.create(VM);
    defer gpa.destroy(vm);

    // 2. Cross-link them so the tracker knows which VM to report to
    gc_tracker.* = GcTrackingAllocator.init(vm, gpa);

    // 3. Initialize the VM logic using the tracking allocator
    try vm.init(gc_tracker.allocator(), std.heap.page_allocator);
    defer vm.deinit();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len == 1) {
        try repl(vm);
    } else if (args.len == 2) {
        try runFile(vm, args[1]);
    } else {
        std.debug.print("Usage: zlox [path]\n", .{});
        std.process.exit(64);
    }
}
