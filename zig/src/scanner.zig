const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF,
};

pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: i32,

    pub fn init(source: []const u8) Scanner {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn deinit(self: *Scanner) void {
        _ = self;
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            c == '_';
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn makeToken(self: *Scanner, @"type": TokenType) Token {
        return .{
            .type = @"type",
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Scanner, message: []const u8) Token {
        return .{
            .type = .TOKEN_ERROR,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // A comment goes until the end of the line.
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => {
                    return;
                },
            }
        }
    }

    fn checkKeyword(self: *Scanner, start: usize, rest: []const u8, @"type": TokenType) TokenType {
        if (self.current - self.start == start + rest.len and
            std.mem.eql(
                u8,
                self.source[self.start + start .. self.current],
                rest,
            ))
        {
            return @"type";
        }
        return .TOKEN_IDENTIFIER;
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword(1, "nd", .TOKEN_AND),
            'c' => self.checkKeyword(1, "lass", .TOKEN_CLASS),
            'e' => self.checkKeyword(1, "lse", .TOKEN_ELSE),
            'f' => {
                if (self.current - self.start > 1) {
                    return switch (self.source[self.start + 1]) {
                        'a' => self.checkKeyword(2, "lse", .TOKEN_FALSE),
                        'o' => self.checkKeyword(2, "r", .TOKEN_FOR),
                        'u' => self.checkKeyword(2, "n", .TOKEN_FUN),
                        else => .TOKEN_IDENTIFIER,
                    };
                } else {
                    return .TOKEN_IDENTIFIER;
                }
            },
            'i' => self.checkKeyword(1, "f", .TOKEN_IF),
            'n' => self.checkKeyword(1, "il", .TOKEN_NIL),
            'o' => self.checkKeyword(1, "r", .TOKEN_OR),
            'p' => self.checkKeyword(1, "rint", .TOKEN_PRINT),
            'r' => self.checkKeyword(1, "eturn", .TOKEN_RETURN),
            's' => self.checkKeyword(1, "uper", .TOKEN_SUPER),
            't' => {
                if (self.current - self.start > 1) {
                    return switch (self.source[self.start + 1]) {
                        'h' => self.checkKeyword(2, "is", .TOKEN_THIS),
                        'r' => self.checkKeyword(2, "ue", .TOKEN_TRUE),
                        else => .TOKEN_IDENTIFIER,
                    };
                } else {
                    return .TOKEN_IDENTIFIER;
                }
            },
            'v' => self.checkKeyword(1, "ar", .TOKEN_VAR),
            'w' => self.checkKeyword(1, "hile", .TOKEN_WHILE),
            else => .TOKEN_IDENTIFIER,
        };
    }

    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(self.identifierType());
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Look for a fractional part.
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "."
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.makeToken(.TOKEN_NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.errorToken("Unterminated string.");
        }

        // The closing ".
        _ = self.advance();
        return self.makeToken(.TOKEN_STRING);
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.TOKEN_EOF);

        const c = self.advance();
        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();

        return switch (c) {
            '(' => self.makeToken(.TOKEN_LEFT_PAREN),
            ')' => self.makeToken(.TOKEN_RIGHT_PAREN),
            '{' => self.makeToken(.TOKEN_LEFT_BRACE),
            '}' => self.makeToken(.TOKEN_RIGHT_BRACE),
            ';' => self.makeToken(.TOKEN_SEMICOLON),
            ',' => self.makeToken(.TOKEN_COMMA),
            '.' => self.makeToken(.TOKEN_DOT),
            '-' => self.makeToken(.TOKEN_MINUS),
            '+' => self.makeToken(.TOKEN_PLUS),
            '/' => self.makeToken(.TOKEN_SLASH),
            '*' => self.makeToken(.TOKEN_STAR),
            '!' => self.makeToken(if (self.match('=')) .TOKEN_BANG_EQUAL else .TOKEN_BANG),
            '=' => self.makeToken(if (self.match('=')) .TOKEN_EQUAL_EQUAL else .TOKEN_EQUAL),
            '<' => self.makeToken(if (self.match('=')) .TOKEN_LESS_EQUAL else .TOKEN_LESS),
            '>' => self.makeToken(if (self.match('=')) .TOKEN_GREATER_EQUAL else .TOKEN_GREATER),
            '"' => self.string(),
            else => self.errorToken("Unexpected character."),
        };
    }
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8, // refer to source
    line: i32,

    pub fn init(@"type": TokenType, lexeme: []const u8, line: i32) Token {
        return .{
            .type = @"type",
            .lexeme = lexeme,
            .line = line,
        };
    }
};
