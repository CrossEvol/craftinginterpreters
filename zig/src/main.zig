const std = @import("std");
const Io = std.Io;

const Chunk = @import("chunk.zig").Chunk;
const disassembleChunk = @import("debug.zig").disassembleChunk;
const OpCode = @import("chunk.zig").OpCode;
const VM = @import("vm.zig").VM;

fn repl(allocator: std.mem.Allocator, vm: *VM) !void {
    var threaded: std.Io.Threaded = .init(allocator, .{});
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
            std.debug.print("\n{s}\n", .{@errorName(err)});
            break;
        };

        _ = vm.interpret(line);
    }
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) []const u8 {
    var threaded: std.Io.Threaded = .init(allocator, .{});
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
    const data = allocator.alloc(u8, file_size) catch |err| {
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

fn runFile(allocator: std.mem.Allocator, vm: *VM, path: []const u8) void {
    const source = readFile(allocator, path);
    defer allocator.free(source);

    const result = vm.interpret(source);
    if (result == .INTERPRET_COMPILE_ERROR) std.process.exit(65);
    if (result == .INTERPRET_RUNTIME_ERROR) std.process.exit(70);
}

pub fn main() !void {
    // In order to allocate memory we must construct an `Allocator` instance.
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit(); // This checks for leaks.
    const allocator = debug_allocator.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try repl(allocator, &vm);
    } else if (args.len == 2) {
        runFile(allocator, &vm, args[1]);
    } else {
        std.debug.print("Usage: zlox [path]\n", .{});
        std.process.exit(64);
    }
}
