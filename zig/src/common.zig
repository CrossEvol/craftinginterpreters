const std = @import("std");

pub var DEBUG_TRACE_EXECUTION = false;
pub var DEBUG_PRINT_CODE = false;
pub var DEBUG_STRESS_GC = false;
pub var DEBUG_LOG_GC = false;
pub const UINT8_COUNT = std.math.maxInt(u8) + 1;
