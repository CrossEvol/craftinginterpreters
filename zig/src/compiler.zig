const std = @import("std");

const asObj = @import("value.zig").Value.asObj;
const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const disassembleChunk = @import("debug.zig").disassembleChunk;
const objVal = @import("value.zig").Value.objVal;
const OpCode = @import("chunk.zig").OpCode;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Value = @import("value.zig").Value;
const numberVal = Value.numberVal;
const VM = @import("vm.zig").VM;

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    pub fn init() Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
    }
};

const Precedence = enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY,
};

pub const Compiler = struct {
    scanner: Scanner,
    compiling_chunk: *Chunk,
    parser: Parser,
    vm: *VM, // borrowed

    pub fn init(vm: *VM) Compiler {
        return .{
            .scanner = undefined,
            .compiling_chunk = undefined,
            .parser = Parser.init(),
            .vm = vm,
        };
    }

    const ParseFn = *const fn (*Compiler) void;

    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?ParseFn,
        precedence: Precedence,

        pub fn init(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
            return .{
                .prefix = prefix,
                .infix = infix,
                .precedence = precedence,
            };
        }
    };

    const rules = std.EnumArray(TokenType, ParseRule).init(.{
        .TOKEN_LEFT_PAREN = ParseRule.init(grouping, null, .PREC_NONE),
        .TOKEN_RIGHT_PAREN = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_LEFT_BRACE = ParseRule.init(null, null, .PREC_NONE), // [big]
        .TOKEN_RIGHT_BRACE = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_COMMA = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_DOT = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_MINUS = ParseRule.init(unary, binary, .PREC_TERM),
        .TOKEN_PLUS = ParseRule.init(null, binary, .PREC_TERM),
        .TOKEN_SEMICOLON = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_SLASH = ParseRule.init(null, binary, .PREC_FACTOR),
        .TOKEN_STAR = ParseRule.init(null, binary, .PREC_FACTOR),
        .TOKEN_BANG = ParseRule.init(unary, null, .PREC_NONE),
        .TOKEN_BANG_EQUAL = ParseRule.init(null, binary, .PREC_EQUALITY),
        .TOKEN_EQUAL = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_EQUAL_EQUAL = ParseRule.init(null, binary, .PREC_EQUALITY),
        .TOKEN_GREATER = ParseRule.init(null, binary, .PREC_COMPARISON),
        .TOKEN_GREATER_EQUAL = ParseRule.init(null, binary, .PREC_COMPARISON),
        .TOKEN_LESS = ParseRule.init(null, binary, .PREC_COMPARISON),
        .TOKEN_LESS_EQUAL = ParseRule.init(null, binary, .PREC_COMPARISON),
        .TOKEN_IDENTIFIER = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_STRING = ParseRule.init(string, null, .PREC_NONE),
        .TOKEN_NUMBER = ParseRule.init(number, null, .PREC_NONE),
        .TOKEN_AND = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_CLASS = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_ELSE = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_FALSE = ParseRule.init(literal, null, .PREC_NONE),
        .TOKEN_FOR = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_FUN = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_IF = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_NIL = ParseRule.init(literal, null, .PREC_NONE),
        .TOKEN_OR = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_PRINT = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_RETURN = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_SUPER = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_THIS = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_TRUE = ParseRule.init(literal, null, .PREC_NONE),
        .TOKEN_VAR = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_WHILE = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_ERROR = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_EOF = ParseRule.init(null, null, .PREC_NONE),
    });

    fn currentChunk(self: *Compiler) *Chunk {
        return self.compiling_chunk;
    }

    fn errorAt(self: *Compiler, token: Token, message: []const u8) void {
        if (self.parser.panic_mode) return;
        self.parser.panic_mode = true;
        std.debug.print("[line {d}] Error", .{@as(usize, @intCast(token.line))});

        if (token.type == .TOKEN_EOF) {
            std.debug.print(" at end", .{});
        } else if (token.type == .TOKEN_ERROR) {
            // Nothing.
        } else {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        self.parser.had_error = true;
    }

    fn @"error"(self: *Compiler, message: []const u8) void {
        self.errorAt(self.parser.previous, message);
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) void {
        self.errorAt(self.parser.current, message);
    }

    fn advance(self: *Compiler) void {
        self.parser.previous = self.parser.current;

        while (true) {
            self.parser.current = self.scanner.scanToken();
            if (self.parser.current.type != .TOKEN_ERROR) break;

            self.errorAtCurrent(self.parser.current.lexeme);
        }
    }

    fn consume(self: *Compiler, @"type": TokenType, message: []const u8) void {
        if (self.parser.current.type == @"type") {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    /// (byte : u8 | OpCode)
    fn emitByte(self: *Compiler, byte: anytype) void {
        const Byte = @TypeOf(byte);
        const byte_type_info = @typeInfo(Byte);
        const valid = switch (byte_type_info) {
            .int => |info| info.signedness == .unsigned and info.bits == 8,
            .@"enum" => |_| Byte == OpCode,
            else => false,
        };
        if (!valid) {
            @panic("byte must be an unsigned 8-bit integer or an OpCode enum");
        }

        const value: u8 = switch (byte_type_info) {
            .int, .comptime_int => byte,
            .@"enum" => @intFromEnum(byte),
            else => unreachable,
        };

        self.currentChunk().write(value, self.parser.previous.line) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
    }

    // (byte1 : u8 | OpCode, byte2 : u8 | OpCode)
    fn emitBytes(self: *Compiler, byte1: anytype, byte2: anytype) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitReturn(self: *Compiler) void {
        self.emitByte(OpCode.OP_RETURN);
    }

    fn makeConstant(self: *Compiler, value: Value) u8 {
        const constants = self.currentChunk().addConstant(value);
        if (constants > std.math.maxInt(u8)) {
            self.@"error"("Too many constants in one chunk.");
            return 0;
        }

        return @intCast(constants);
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        self.emitBytes(OpCode.OP_CONSTANT, self.makeConstant(value));
    }

    fn endCompiler(self: *Compiler) void {
        self.emitReturn();
        if (common.DEBUG_PRINT_CODE) {
            if (!self.parser.had_error) {
                disassembleChunk(self.currentChunk(), "code");
            }
        }
    }

    fn expression(self: *Compiler) void {
        self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    fn getRule(@"type": TokenType) ParseRule {
        return rules.get(@"type");
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) void {
        self.advance();
        const option_prefix_rule = getRule(self.parser.previous.type).prefix;
        if (option_prefix_rule) |prefix_rule| {
            prefix_rule(self);
            while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
                self.advance();
                const option_infix_rule = getRule(self.parser.previous.type).infix;
                if (option_infix_rule) |infix_rule| {
                    infix_rule(self);
                }
            }
        } else {
            self.@"error"("Expect expression.");
            return;
        }
    }

    fn binary(self: *Compiler) void {
        const operator_type = self.parser.previous.type;
        const rule = getRule(operator_type);
        self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operator_type) {
            .TOKEN_BANG_EQUAL => self.emitBytes(OpCode.OP_EQUAL, OpCode.OP_NOT),
            .TOKEN_EQUAL_EQUAL => self.emitByte(OpCode.OP_EQUAL),
            .TOKEN_GREATER => self.emitByte(OpCode.OP_GREATER),
            .TOKEN_GREATER_EQUAL => self.emitBytes(OpCode.OP_LESS, OpCode.OP_NOT),
            .TOKEN_LESS => self.emitByte(OpCode.OP_LESS),
            .TOKEN_LESS_EQUAL => self.emitBytes(OpCode.OP_GREATER, OpCode.OP_NOT),
            .TOKEN_PLUS => self.emitByte(OpCode.OP_ADD),
            .TOKEN_MINUS => self.emitByte(OpCode.OP_SUBTRACT),
            .TOKEN_STAR => self.emitByte(OpCode.OP_MULTIPLY),
            .TOKEN_SLASH => self.emitByte(OpCode.OP_DIVIDE),
            else => {}, // Unreachable.
        }
    }

    fn literal(self: *Compiler) void {
        switch (self.parser.previous.type) {
            .TOKEN_FALSE => self.emitByte(OpCode.OP_FALSE),
            .TOKEN_NIL => self.emitByte(OpCode.OP_NIL),
            .TOKEN_TRUE => self.emitByte(OpCode.OP_TRUE),
            else => {}, // Unreachable.
        }
    }

    fn grouping(self: *Compiler) void {
        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn number(self: *Compiler) void {
        const value = std.fmt.parseFloat(f64, self.parser.previous.lexeme) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
        self.emitConstant(numberVal(value));
    }

    fn string(self: *Compiler) void {
        const lexeme = self.parser.previous.lexeme;
        var obj_string = self.vm.copyString(lexeme[1 .. lexeme.len - 1]) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
        _ = &obj_string;

        const obj_constant = objVal(obj_string.asObj());
        self.emitConstant(obj_constant);
    }

    fn unary(self: *Compiler) void {
        const operator_type = self.parser.previous.type;

        // Compile the operand.
        self.parsePrecedence(.PREC_UNARY);

        // Emit the operator instruction.
        switch (operator_type) {
            .TOKEN_BANG => self.emitByte(OpCode.OP_NOT),
            .TOKEN_MINUS => self.emitByte(OpCode.OP_NEGATE),
            else => {}, // Unreachable.
        }
    }

    pub fn compile(self: *Compiler, source: []const u8, chunk: *Chunk) bool {
        self.scanner = Scanner.init(source);
        self.compiling_chunk = chunk;

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.advance();
        self.expression();
        self.consume(.TOKEN_EOF, "Expect end of expression.");
        self.endCompiler();

        return !self.parser.had_error;
    }
};
