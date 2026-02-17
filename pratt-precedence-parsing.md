```rust
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
```

`(-1 + 2) * 3 - -4`


pattern of parser is [ previous, current ], show the status before call `getRule`
from top to bottom are .{ parser, function_call, generated_code ,}

```rust
[ undef, undef ]
compile()
{}
    [ undef, ( ]
    expression()
    {}
        [ ( , - ]
        parsePrecedence(.PREC_ASSIGNMENT) 
        {}
            [ ( , - ]
            grouping()
            {}
                [ ( , - ]
                expression()
                {}
                    [ - , 1 ]
                    parsePrecedence(.PREC_ASSIGNMENT) 
                    {}
                        [ - , 1 ]
                        unary(-)
                        {}
                            [ 1 , + ]
                            parsePrecedence(.PREC_UNARY) 
                            {}
                                [ 1 , + ]
                                number(1)
                                { 1 }
                            [ 1 , + ]
                            parsePrecedence(.PREC_UNARY) :: unary <= term -> false
                            { 1 }
                        [ 1 , + ]
                        unary(-)
                        { 1 , - }
                    [ 1 , + ] -> [ + , 2]
                    parsePrecedence(.PREC_ASSIGNMENT) :: assign <= term -> true
                    { 1 , - }
                        [ + , 2]
                        binary(+)
                        { 1 , - }
                            [ + , 2] -> [ 2 , ) ]
                            parsePrecedence(.PREC_FACTOR := .PREC_TERM + 1)
                            { 1 , - }
                                [ 2 , ) ]
                                number(2)
                                { 1 , - , 2}
                            [ 2 , ) ]
                            parsePrecedence(.PREC_FACTOR) :: factor <= none -> false
                            { 1 , - , 2}
                        [ 2 , ) ]
                        binary(+)
                        { 1 , - , 2} -> { 1 , - , 2, + }
                    [ 2 , ) ]
                    parsePrecedence(.PREC_ASSIGNMENT) :: assign <= none -> false
                    { 1 , - , 2, + }
                    [ 2 , ) ]
                    parsePrecedence(.PREC_ASSIGNMENT) :: assign <= none -> false
                    { 1 , - , 2, + }
                [ 2 , ) ]
                expression()
                { 1 , - , 2, + }
            [ 2 , ) ] -> [ ) , * ]
            grouping()
            { 1 , - , 2, + }
        [ ) , * ] -> [ * , 3 ]
        parsePrecedence(.PREC_ASSIGNMENT) :: assign <= factor -> true
        { 1 , - , 2, + }
            [ * , 3 ]
            binary(*)
            { 1 , - , 2, + }
                [ * , 3 ] -> [ 3 , - ] 
                parsePrecedence(.PREC_UNARY := .PREC_FACTOR + 1) 
                { 1 , - , 2, + }
                    [ 3 , - ]
                    number(3) 
                    { 1 , - , 2, + , 3 }
                [ 3 , - ] 
                parsePrecedence(.PREC_UNARY) :: unary <= term -> false
                { 1 , - , 2, + , 3 }
            [ 3 , - ] 
            binary(*)
            { 1 , - , 2, + , 3 , *}
        [ 3 , - ] -> [ - , - ]
        parsePrecedence(.PREC_ASSIGNMENT) :: assign <= term -> true
        { 1 , - , 2, + , 3 , *}
                [ - , - ]
                binary(-)
                { 1 , - , 2, + , 3 , *}
                    [ - , - ] -> [ - , 4 ]
                    parsePrecedence(.PREC_FACTOR := .PREC_TERM + 1)
                    { 1 , - , 2, + , 3 , *}
                        [ - , 4 ]
                        unary(-)
                        { 1 , - , 2, + , 3 , *}
                            [ - , 4 ] -> [ 4 , eof ]
                            parsePrecedence(.PREC_UNARY)
                            { 1 , - , 2, + , 3 , *}
                                [ 4 , eof ]
                                number(4)
                                { 1 , - , 2, + , 3 , * , 4}
                            [ 4 , eof ]
                            parsePrecedence(.PREC_UNARY) :: unary <= eof -> false
                            { 1 , - , 2, + , 3 , * , 4}
                        [ 4 , eof ]
                        unary(-)
                        { 1 , - , 2, + , 3 , * , 4 , -}
                    [ 4 , eof ]
                    parsePrecedence(.PREC_FACTOR) :: factor <= eof -> false
                    { 1 , - , 2, + , 3 , * , 4 , -}
                [ 4 , eof ]
                binary(-)
                { 1 , - , 2, + , 3 , * , 4 , - , -}
        [ 4 , eof ]
        parsePrecedence(.PREC_ASSIGNMENT) :: assign <= eof -> false
        { 1 , - , 2, + , 3 , * , 4 , - , -}
    [ 4 , eof ]
    expression()
    { 1 , - , 2, + , 3 , * , 4 , - , -}
[ 4 , eof ]
compile()
{}                           
```