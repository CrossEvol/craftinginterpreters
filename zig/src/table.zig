const std = @import("std");

const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const nil_val = Value.nil_val;
const isNil = Value.isNil;
const boolVal = Value.boolVal;

const TABLE_MAX_LOAD = 0.75;

const Entry = struct {
    key: ?*ObjString,
    value: Value,

    pub fn init() Entry {
        return .{
            .key = null,
            .value = nil_val,
        };
    }

    pub fn deinit(self: *const Entry, allocator: std.mem.Allocator) void {
        if (self.key) |key| {
            key.deinit(allocator);
            allocator.destroy(key);
        }
    }
};

pub const Table = struct {
    entries: []Entry,
    count: usize,
    capacity: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Table {
        return .{
            .count = 0,
            .capacity = 0,
            .entries = &.{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
        self.* = Table.init(self.allocator);
    }

    fn findEntry(entries: []Entry, key: *ObjString) *Entry {
        var index = key.hash % entries.len;
        var tomb_stone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];

            if (entry.key == null) {
                if (isNil(entry.value)) {
                    // Empty entry.
                    return if (tomb_stone) |t| t else entry;
                } else {
                    // We found a tombstone.
                    if (tomb_stone == null) tomb_stone = entry;
                }
            } else if (entry.key == key) {
                // We found the key.
                return entry;
            }

            index = (index + 1) % entries.len;
        }
    }

    /// -> (value : Value, ok : book)
    /// WARNING: this function can be not faithful to c impl, it do not use outer param, but return tuple
    pub fn get(self: *Table, key: *ObjString) struct { Value, bool } {
        if (self.count == 0) return .{ nil_val, false };

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return .{ nil_val, false };

        return .{ entry.value, true };
    }

    fn adjustCapacity(self: *Table, capacity: usize) void {
        const entries = self.allocator.alloc(Entry, capacity) catch |err| {
            std.debug.print("{s}\n", .{@errorName(err)});
            @panic("OOM");
        };
        for (0..entries.len) |i| {
            entries[i] = Entry.init();
        }

        self.count = 0;
        for (self.entries) |entry| {
            const key = entry.key orelse continue;
            const dest = findEntry(entries, key);
            dest.key = key;
            dest.value = entry.value;
            self.count += 1;
        }

        self.allocator.free(self.entries);
        self.entries = entries;
        self.capacity = capacity;
    }

    pub fn set(self: *Table, key: *ObjString, value: Value) bool {
        const threshold: usize = @intFromFloat(@as(f64, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD);
        if (self.count + 1 > threshold) {
            const capacity = growCapacity(self.capacity);
            self.adjustCapacity(capacity);
        }

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key and isNil(entry.value)) self.count += 1;

        entry.key = key;
        entry.value = value;
        return is_new_key;
    }

    pub fn delete(self: *Table, key: *ObjString) bool {
        if (self.count == 0) return false;

        // Find the entry.
        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // Place a tombstone in the entry.
        entry.key = null;
        entry.value = boolVal(true);
        return true;
    }

    pub fn addAll(from: *Table, to: *Table) void {
        for (from.entries) |entry| {
            if (entry.key) |key| {
                _ = to.set(key, entry.value);
            }
        }
    }

    pub fn findString(self: *Table, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;

        var index = hash % self.capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |k| {
                if (k.chars.len == chars.len and k.hash == hash and std.mem.eql(u8, k.chars, chars)) {
                    // We found it.
                    return k;
                }
            } else {
                // Stop if we find an empty non-tombstone entry.
                if (isNil(entry.value)) return null;
            }

            index = (index + 1) % self.capacity;
        }
    }
};

fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}
