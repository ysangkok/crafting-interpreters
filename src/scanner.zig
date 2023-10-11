const std = @import("std");

pub const Scanner = struct {
    start: u16,
    current: u16,
    line: u16,
    buf: []const u8,
};

pub const Token = struct {
    typ: TokenType,
    start: u16,
    length: u16,
    line: u16,
};

pub const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF,
};

fn makeToken(scanner: *Scanner, typ: TokenType) !Token {
    return Token{
        .typ = typ,
        .start = scanner.start,
        .length = (scanner.current - scanner.start),
        .line = scanner.line,
    };
}

fn advance(scanner: *Scanner) u8 {
    scanner.current += 1;
    return scanner.buf[scanner.current - 1];
}

fn isAtEnd(scanner: *Scanner) bool {
    return scanner.current >= scanner.buf.len;
}

fn match(scanner: *Scanner, comptime expected: comptime_int) bool {
    if (isAtEnd(scanner)) {
        return false;
    }
    if (scanner.buf[scanner.current] != expected) return false;
    scanner.current += 1;
    return true;
}

fn peek(scanner: *Scanner) u8 {
    return scanner.buf[scanner.current];
}

fn peekNext(scanner: *Scanner) u8 {
    if (scanner.current + 1 >= scanner.buf.len) return 0;
    return scanner.buf[scanner.current + 1];
}

fn skipWhitespace(scanner: *Scanner) void {
    while (!isAtEnd(scanner)) {
        var c: u8 = peek(scanner);
        switch (c) {
            ' ', '\r', '\t' => _ = advance(scanner),
            '\n' => {
                scanner.line += 1;
                _ = advance(scanner);
            },
            '/' => {
                if (peekNext(scanner) == '/') {
                    // A comment goes until the end of the line.
                    while (peek(scanner) != '\n' and !isAtEnd(scanner))
                        _ = advance(scanner);
                } else return;
            },
            else => return,
        }
    }
}

fn string(scanner: *Scanner) !Token {
    while (peek(scanner) != '"' and !isAtEnd(scanner)) {
        if (peek(scanner) == '\n') scanner.line += 1;
        _ = advance(scanner);
    }

    if (isAtEnd(scanner)) @panic("Unterminated string.");

    // The closing quote.
    _ = advance(scanner);
    return try makeToken(scanner, TokenType.STRING);
}

fn number(scanner: *Scanner) !Token {
    while (isDigit(peek(scanner))) {
        _ = advance(scanner);
    }

    // Look for a fractional part.
    if (peek(scanner) == '.' and isDigit(peekNext(scanner))) {
        // Consume the ".".
        _ = advance(scanner);

        while (isDigit(peek(scanner))) {
            _ = advance(scanner);
        }
    }

    return makeToken(scanner, TokenType.NUMBER);
}

fn identifierType(scanner: *Scanner) TokenType {
    return switch (scanner.buf[scanner.start]) {
        'a' => checkKeyword(scanner, 1, "nd", TokenType.AND),
        'c' => checkKeyword(scanner, 1, "lass", TokenType.CLASS),
        'e' => checkKeyword(scanner, 1, "lse", TokenType.ELSE),
        'f' => if (scanner.current - scanner.start > 1)
            switch (scanner.buf[scanner.start + 1]) {
                'a' => checkKeyword(scanner, 2, "lse", TokenType.FALSE),
                'o' => checkKeyword(scanner, 2, "r", TokenType.FOR),
                'u' => checkKeyword(scanner, 2, "n", TokenType.FUN),
                else => TokenType.IDENTIFIER,
            }
        else
            TokenType.IDENTIFIER,
        'i' => checkKeyword(scanner, 1, "f", TokenType.IF),
        'n' => checkKeyword(scanner, 1, "il", TokenType.NIL),
        'o' => checkKeyword(scanner, 1, "r", TokenType.OR),
        'p' => checkKeyword(scanner, 1, "rint", TokenType.PRINT),
        'r' => checkKeyword(scanner, 1, "eturn", TokenType.RETURN),
        's' => checkKeyword(scanner, 1, "uper", TokenType.SUPER),
        't' => if (scanner.current - scanner.start > 1)
            switch (scanner.buf[scanner.start + 1]) {
                'h' => checkKeyword(scanner, 2, "is", TokenType.THIS),
                'r' => checkKeyword(scanner, 2, "ue", TokenType.TRUE),
                else => TokenType.IDENTIFIER,
            }
        else
            TokenType.IDENTIFIER,
        'v' => checkKeyword(scanner, 1, "ar", TokenType.VAR),
        'w' => checkKeyword(scanner, 1, "hile", TokenType.WHILE),
        else => TokenType.IDENTIFIER,
    };
}

fn identifier(scanner: *Scanner) !Token {
    while (isAlpha(peek(scanner)) or isDigit(peek(scanner))) _ = advance(scanner);
    return makeToken(scanner, identifierType(scanner));
}

pub fn scanToken(scanner: *Scanner) Token {
    // scanToken
    skipWhitespace(scanner);
    scanner.start = scanner.current;
    if (isAtEnd(scanner)) return try makeToken(scanner, TokenType.EOF);
    var c = advance(scanner);
    const token = try switch (c) {
        '0'...'9' => number(scanner),
        '(' => makeToken(scanner, TokenType.LEFT_PAREN),
        ')' => makeToken(scanner, TokenType.RIGHT_PAREN),
        '{' => makeToken(scanner, TokenType.LEFT_BRACE),
        '}' => makeToken(scanner, TokenType.RIGHT_BRACE),
        ';' => makeToken(scanner, TokenType.SEMICOLON),
        ',' => makeToken(scanner, TokenType.COMMA),
        '.' => makeToken(scanner, TokenType.DOT),
        '-' => makeToken(scanner, TokenType.MINUS),
        '+' => makeToken(scanner, TokenType.PLUS),
        '/' => makeToken(scanner, TokenType.SLASH),
        '*' => makeToken(scanner, TokenType.STAR),
        '!' => makeToken(scanner, if (match(scanner, '=')) TokenType.BANG_EQUAL else TokenType.BANG),
        '=' => makeToken(scanner, if (match(scanner, '=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
        '<' => makeToken(scanner, if (match(scanner, '=')) TokenType.LESS_EQUAL else TokenType.LESS),
        '>' => makeToken(scanner, if (match(scanner, '=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
        '"' => string(scanner),
        else => if (isAlpha(c))
            identifier(scanner)
        else
            std.debug.panic("unknown char '{s}', scanner state: start={d} current={d} line={d}", .{ [1]u8{c}, scanner.start, scanner.current, scanner.line }),
    };
    return token;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return c >= 'a' and c <= 'z' or c >= 'A' and c <= 'Z' or c == '_';
}

fn checkKeyword(scanner: *Scanner, start: u16, rest: []const u8, typ: TokenType) TokenType {
    const begin = scanner.buf[scanner.start + start ..];
    if (std.mem.startsWith(u8, begin, rest)) {
        return typ;
    }

    return TokenType.IDENTIFIER;
}
