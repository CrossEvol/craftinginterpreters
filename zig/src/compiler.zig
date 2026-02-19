const std = @import("std");

const asObj = @import("value.zig").Value.asObj;
const Chunk = @import("chunk.zig").Chunk;
const common = @import("common.zig");
const UINT8_COUNT = common.UINT8_COUNT;
const disassembleChunk = @import("debug.zig").disassembleChunk;
const objVal = @import("value.zig").Value.objVal;
const OpCode = @import("chunk.zig").OpCode;
const Scanner = @import("scanner.zig").Scanner;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Value = @import("value.zig").Value;
const numberVal = Value.numberVal;
const VM = @import("vm.zig").VM;

const UINT16_MAX = std.math.maxInt(u16);

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

const ParseFn = *const fn (*Compiler, can_assign: bool) void;

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

const Local = struct {
    name: Token,
    depth: i32,

    pub fn init(name: Token, depth: i32) Local {
        return .{
            .name = name,
            .depth = depth,
        };
    }
};

const Kompiler = struct {
    locals: [UINT8_COUNT]Local,
    local_count: i32,
    scope_depth: i32,

    pub fn init() Kompiler {
        return .{
            .locals = undefined,
            .local_count = 0,
            .scope_depth = 0,
        };
    }
};

fn identifiersEqual(a: Token, b: Token) bool {
    if (a.lexeme.len != b.lexeme.len) return false;
    return std.mem.eql(u8, a.lexeme, b.lexeme);
}

pub const Compiler = struct {
    current: *Kompiler,
    parser: Parser,
    scanner: Scanner,
    vm: *VM,
    compiling_chunk: *Chunk,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, vm: *VM) Compiler {
        return .{
            .current = undefined,
            .scanner = undefined,
            .parser = Parser.init(),
            .vm = vm,
            .compiling_chunk = undefined,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.allocator.destroy(self.current);
    }

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

    fn check(self: *Compiler, @"type": TokenType) bool {
        return self.parser.current.type == @"type";
    }

    fn match(self: *Compiler, @"type": TokenType) bool {
        if (!self.check(@"type")) return false;
        self.advance();
        return true;
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

    fn emitLoop(self: *Compiler, loop_start: i32) void {
        self.emitByte(OpCode.OP_LOOP);

        const offset = self.currentChunk().count() - loop_start + 2;
        if (offset > UINT16_MAX) {
            self.@"error"("Loop body too large.");
        }

        self.emitByte(@as(u8, @intCast((offset >> 8) & 0xff)));
        self.emitByte(@as(u8, @intCast(offset & 0xff)));
    }

    fn emitJump(self: *Compiler, instruction: anytype) i32 {
        self.emitByte(instruction);
        self.emitByte(@as(u8, 0xff));
        self.emitByte(@as(u8, 0xff));
        return self.currentChunk().count() - 2;
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

    fn patchJump(self: *Compiler, offset: i32) void {
        // -2 to adjust for the bytecode for the jump offset itself.
        const jump = self.currentChunk().count() - offset - 2;

        if (jump > UINT16_MAX) {
            self.@"error"("Too much code to jump over.");
        }

        self.currentChunk().code.items[@intCast(offset)] = @intCast((jump >> 8) & 0xff);
        self.currentChunk().code.items[@intCast(offset + 1)] = @intCast(jump & 0xff);
    }

    fn endCompiler(self: *Compiler) void {
        self.emitReturn();
        if (common.DEBUG_PRINT_CODE) {
            if (!self.parser.had_error) {
                disassembleChunk(self.currentChunk(), "code");
            }
        }
    }

    fn beginScope(self: *Compiler) void {
        self.current.scope_depth += 1;
    }

    fn endScope(self: *Compiler) void {
        self.current.scope_depth -= 1;

        while (self.current.local_count > 0 and self.current.locals[@intCast(self.current.local_count - 1)].depth > self.current.scope_depth) {
            self.emitByte(OpCode.OP_POP);
            self.current.local_count -= 1;
        }
    }

    /// https://craftinginterpreters.com/image/jumping-back-and-forth/and.png
    fn and_(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

        const end_jump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);

        self.emitByte(OpCode.OP_POP);
        self.parsePrecedence(.PREC_AND);

        self.patchJump(end_jump);
    }

    fn binary(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

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

    fn identifierConstant(self: *Compiler, name: Token) u8 {
        return self.makeConstant(objVal(self.vm.copyString(name.lexeme).asObj()));
    }

    fn resolveLocal(self: *Compiler, name: Token) i32 {
        var i = self.current.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.current.locals[@intCast(i)];
            if (identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    self.@"error"("Can't read local variable in its own initializer.");
                }
                return i;
            }
        }
        return -1;
    }

    fn addLocal(self: *Compiler, name: Token) void {
        if (self.current.local_count == UINT8_COUNT) {
            self.@"error"("Too many local variables in function.");
            return;
        }

        self.current.locals[@intCast(self.current.local_count)] = Local.init(name, -1);
        self.current.local_count += 1;
    }

    fn declareVariable(self: *Compiler) void {
        if (self.current.scope_depth == 0) return;

        const name = self.parser.previous;
        var i = self.current.local_count - 1;
        while (i >= 0) : (i -= 1) {
            const local = self.current.locals[@intCast(i)];

            if (local.depth != -1 and local.depth < self.current.scope_depth) {
                break; // [negative]
            }

            if (identifiersEqual(name, local.name)) {
                self.@"error"("Already a variable with this name in this scope.");
            }
        }

        self.addLocal(name);
    }

    fn parseVariable(self: *Compiler, error_message: []const u8) u8 {
        self.consume(.TOKEN_IDENTIFIER, error_message);

        self.declareVariable();
        if (self.current.scope_depth > 0) return 0;

        return self.identifierConstant(self.parser.previous);
    }

    fn markInitialized(self: *Compiler) void {
        self.current.locals[@intCast(self.current.local_count - 1)].depth = self.current.scope_depth;
    }

    fn defineVariable(self: *Compiler, global: u8) void {
        if (self.current.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        self.emitBytes(OpCode.OP_DEFINE_GLOBAL, global);
    }

    fn literal(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

        switch (self.parser.previous.type) {
            .TOKEN_FALSE => self.emitByte(OpCode.OP_FALSE),
            .TOKEN_NIL => self.emitByte(OpCode.OP_NIL),
            .TOKEN_TRUE => self.emitByte(OpCode.OP_TRUE),
            else => {}, // Unreachable.
        }
    }

    fn grouping(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn number(self: *Compiler, can_assign: bool) void {
        _ = can_assign;
        const value = std.fmt.parseFloat(f64, self.parser.previous.lexeme) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
        self.emitConstant(numberVal(value));
    }

    /// https://craftinginterpreters.com/image/jumping-back-and-forth/or.png
    fn or_(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

        const else_jump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        const end_jump = self.emitJump(OpCode.OP_JUMP);

        self.patchJump(else_jump);
        self.emitByte(OpCode.OP_POP);

        self.parsePrecedence(.PREC_OR);
        self.patchJump(end_jump);
    }

    fn string(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

        const lexeme = self.parser.previous.lexeme;
        var obj_string = self.vm.copyString(lexeme[1 .. lexeme.len - 1]);
        _ = &obj_string;

        const obj_constant = objVal(obj_string.asObj());
        self.emitConstant(obj_constant);
    }

    fn namedVariable(self: *Compiler, name: Token, can_assign: bool) void {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;
        var arg = self.resolveLocal(name);

        if (arg != -1) {
            get_op = .OP_GET_LOCAL;
            set_op = .OP_SET_LOCAL;
        } else {
            arg = @intCast(self.identifierConstant(name));
            get_op = .OP_GET_GLOBAL;
            set_op = .OP_SET_GLOBAL;
        }

        if (can_assign and self.match(.TOKEN_EQUAL)) {
            self.expression();
            self.emitBytes(set_op, @as(u8, @intCast(arg)));
        } else {
            self.emitBytes(get_op, @as(u8, @intCast(arg)));
        }
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.namedVariable(self.parser.previous, can_assign);
    }

    fn unary(self: *Compiler, can_assign: bool) void {
        _ = can_assign;

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
        .TOKEN_IDENTIFIER = ParseRule.init(variable, null, .PREC_NONE),
        .TOKEN_STRING = ParseRule.init(string, null, .PREC_NONE),
        .TOKEN_NUMBER = ParseRule.init(number, null, .PREC_NONE),
        .TOKEN_AND = ParseRule.init(null, and_, .PREC_AND),
        .TOKEN_CLASS = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_ELSE = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_FALSE = ParseRule.init(literal, null, .PREC_NONE),
        .TOKEN_FOR = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_FUN = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_IF = ParseRule.init(null, null, .PREC_NONE),
        .TOKEN_NIL = ParseRule.init(literal, null, .PREC_NONE),
        .TOKEN_OR = ParseRule.init(null, or_, .PREC_OR),
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

    fn parsePrecedence(self: *Compiler, precedence: Precedence) void {
        self.advance();
        const option_prefix_rule = getRule(self.parser.previous.type).prefix;
        if (option_prefix_rule) |prefix_rule| {
            const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);

            prefix_rule(self, can_assign);

            while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
                self.advance();
                const option_infix_rule = getRule(self.parser.previous.type).infix;
                if (option_infix_rule) |infix_rule| {
                    infix_rule(self, can_assign);
                }
            }

            if (can_assign and self.match(.TOKEN_EQUAL)) {
                self.@"error"("Invalid assignment target.");
            }
        } else {
            self.@"error"("Expect expression.");
            return;
        }
    }

    fn getRule(@"type": TokenType) ParseRule {
        return rules.get(@"type");
    }

    fn expression(self: *Compiler) void {
        self.parsePrecedence(.PREC_ASSIGNMENT);
    }

    fn block(self: *Compiler) void {
        while (!self.check(.TOKEN_RIGHT_BRACE) and !self.check(.TOKEN_EOF)) {
            self.declaration();
        }

        self.consume(.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }

    fn varDeclaration(self: *Compiler) void {
        const global = self.parseVariable("Expect variable name.");

        if (self.match(.TOKEN_EQUAL)) {
            self.expression();
        } else {
            self.emitByte(OpCode.OP_NIL);
        }

        self.consume(.TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

        self.defineVariable(global);
    }

    fn expressionStatement(self: *Compiler) void {
        self.expression();
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
        self.emitByte(OpCode.OP_POP);
    }

    fn printStatement(self: *Compiler) void {
        self.expression();
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after value.");
        self.emitByte(OpCode.OP_PRINT);
    }

    /// https://craftinginterpreters.com/image/jumping-back-and-forth/for.png
    fn forStatement(self: *Compiler) void {
        self.beginScope();
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
        if (self.match(.TOKEN_SEMICOLON)) {
            // No initializer.
        } else if (self.match(.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loop_start = self.currentChunk().count();
        var exit_jump: i32 = -1;
        if (!self.match(.TOKEN_SEMICOLON)) {
            self.expression();
            self.consume(.TOKEN_SEMICOLON, "Expect ';' after loop condition.");

            // Jump out of the loop if the condition is false.
            exit_jump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
            self.emitByte(OpCode.OP_POP); // Condition.
        }

        if (!self.match(.TOKEN_RIGHT_PAREN)) {
            const body_jump = self.emitJump(OpCode.OP_JUMP);

            const increment_start = self.currentChunk().count();
            self.expression();
            self.emitByte(OpCode.OP_POP);
            self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

            self.emitLoop(loop_start);
            loop_start = increment_start;
            self.patchJump(body_jump);
        }

        self.statement();
        self.emitLoop(loop_start);

        if (exit_jump != -1) {
            self.patchJump(exit_jump);
            self.emitByte(OpCode.OP_POP); // Condition.
        }

        self.endScope();
    }

    /// https://craftinginterpreters.com/image/jumping-back-and-forth/full-if-else.png
    fn ifStatement(self: *Compiler) void {
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const then_jump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        self.emitByte(OpCode.OP_POP);
        self.statement();

        const else_jump = self.emitJump(OpCode.OP_JUMP);

        self.patchJump(then_jump);
        self.emitByte(OpCode.OP_POP);

        if (self.match(.TOKEN_ELSE)) self.statement();
        self.patchJump(else_jump);
    }

    /// https://craftinginterpreters.com/image/jumping-back-and-forth/while.png
    fn whileStatement(self: *Compiler) void {
        const loop_start = self.currentChunk().count();
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const exit_jump = self.emitJump(OpCode.OP_JUMP_IF_FALSE);
        self.emitByte(OpCode.OP_POP);
        self.statement();
        self.emitLoop(loop_start);

        self.patchJump(exit_jump);
        self.emitByte(OpCode.OP_POP);
    }

    fn synchronize(self: *Compiler) void {
        self.parser.panic_mode = false;

        while (self.parser.current.type != .TOKEN_EOF) {
            if (self.parser.previous.type == .TOKEN_SEMICOLON) return;

            switch (self.parser.current.type) {
                .TOKEN_CLASS,
                .TOKEN_FUN,
                .TOKEN_VAR,
                .TOKEN_FOR,
                .TOKEN_IF,
                .TOKEN_WHILE,
                .TOKEN_PRINT,
                .TOKEN_RETURN,
                => return,
                else => {}, // Do nothing.
            }

            self.advance();
        }
    }

    fn declaration(self: *Compiler) void {
        if (self.match(.TOKEN_VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }
        if (self.parser.panic_mode) self.synchronize();
    }

    fn statement(self: *Compiler) void {
        if (self.match(.TOKEN_PRINT)) {
            self.printStatement();
        } else if (self.match(.TOKEN_FOR)) {
            self.forStatement();
        } else if (self.match(.TOKEN_IF)) {
            self.ifStatement();
        } else if (self.match(.TOKEN_WHILE)) {
            self.whileStatement();
        } else if (self.match(.TOKEN_LEFT_BRACE)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    pub fn compile(self: *Compiler, source: []const u8, chunk: *Chunk) bool {
        self.scanner = Scanner.init(source);
        const compiler = self.allocator.create(Kompiler) catch |err| {
            std.debug.print("{s}", .{@errorName(err)});
            @panic(@errorName(err));
        };
        compiler.* = Kompiler.init();
        self.current = compiler;
        self.compiling_chunk = chunk;

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.advance();

        while (!self.match(.TOKEN_EOF)) {
            self.declaration();
        }

        self.endCompiler();
        return !self.parser.had_error;
    }
};
