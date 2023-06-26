const std = @import("std");

const TokenType = enum {
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

const Scanner = struct {
    start: u16,
    current: u16,
    line: u16,
};

const Token = struct {
    typ: TokenType,
    start: u16,
    length: u16,
    line: u16,
};

fn makeToken(scanner: *Scanner, typ: TokenType) !Token {
    var token: Token = undefined;
    token.typ = typ;
    token.start = scanner.start;
    token.length = (scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

fn advance(scanner: *Scanner, buf: []const u8) u8 {
    scanner.current += 1;
    return buf[scanner.current - 1];
}

fn isAtEnd(scanner: *Scanner, buf: []const u8) bool {
    return scanner.current >= buf.len;
}

fn match(scanner: *Scanner, buf: []const u8, comptime expected: comptime_int) bool {
    if (isAtEnd(scanner, buf)) {
        return false;
    }
    if (buf[scanner.current] != expected) return false;
    scanner.current += 1;
    return true;
}

fn peek(scanner: *Scanner, buf: []const u8) u8 {
    return buf[scanner.current];
}

fn skipWhitespace(scanner: *Scanner, buf: []const u8) void {
    while (!isAtEnd(scanner, buf)) {
        var c: u8 = peek(scanner, buf);
        switch (c) {
            ' ', '\r', '\t' => _ = advance(scanner, buf),
            '\n' => {
                scanner.line += 1;
                _ = advance(scanner, buf);
            },
            '/' => {
                if (peekNext(scanner, buf) == '/') {
                    // A comment goes until the end of the line.
                    while (peek(scanner, buf) != '\n' and !isAtEnd(scanner, buf))
                        _ = advance(scanner, buf);
                } else return;
            },
            else => return,
        }
    }
}

fn peekNext(scanner: *Scanner, buf: []const u8) u8 {
    if (isAtEnd(scanner, buf)) return 0;
    return buf[scanner.current];
}

fn string(scanner: *Scanner, buf: []const u8) !Token {
    while (peek(scanner, buf) != '"' and !isAtEnd(scanner, buf)) {
        if (peek(scanner, buf) == '\n') scanner.line += 1;
        _ = advance(scanner, buf);
    }

    if (isAtEnd(scanner, buf)) @panic("Unterminated string.");

    // The closing quote.
    _ = advance(scanner, buf);
    return try makeToken(scanner, TokenType.STRING);
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return c >= 'a' and c <= 'z' or c >= 'A' and c <= 'A' or c == '_';
}

fn number(scanner: *Scanner, buf: []const u8) !Token {
    while (isDigit(peek(scanner, buf))) _ = advance(scanner, buf);

    // Look for a fractional part.
    if (peek(scanner, buf) == '.' and isDigit(peekNext(scanner, buf))) {
        // Consume the ".".
        _ = advance(scanner, buf);

        while (isDigit(peek(scanner, buf))) _ = advance(scanner, buf);
    }

    return makeToken(scanner, TokenType.NUMBER);
}

fn identifier(scanner: *Scanner, buf: []const u8) !Token {
    while (isAlpha(peek(scanner, buf)) or isDigit(peek(scanner, buf))) _ = advance(scanner, buf);
    return makeToken(scanner, identifierType(scanner, buf));
}

fn identifierType(scanner: *Scanner, buf: []const u8) TokenType {
    return switch (buf[scanner.start]) {
        'a' => checkKeyword(scanner, buf, 1, "nd", TokenType.AND),
        'c' => checkKeyword(scanner, buf, 1, "lass", TokenType.CLASS),
        'e' => checkKeyword(scanner, buf, 1, "lse", TokenType.ELSE),
        'f' => if (scanner.current - scanner.start > 1)
            switch (buf[scanner.start + 1]) {
                'a' => checkKeyword(scanner, buf, 2, "lse", TokenType.FALSE),
                'o' => checkKeyword(scanner, buf, 2, "r", TokenType.FOR),
                'u' => checkKeyword(scanner, buf, 2, "n", TokenType.FUN),
                else => TokenType.IDENTIFIER,
            }
        else
            TokenType.IDENTIFIER,
        'i' => checkKeyword(scanner, buf, 1, "f", TokenType.IF),
        'n' => checkKeyword(scanner, buf, 1, "il", TokenType.NIL),
        'o' => checkKeyword(scanner, buf, 1, "r", TokenType.OR),
        'p' => checkKeyword(scanner, buf, 1, "rint", TokenType.PRINT),
        'r' => checkKeyword(scanner, buf, 1, "eturn", TokenType.RETURN),
        's' => checkKeyword(scanner, buf, 1, "uper", TokenType.SUPER),
        't' => if (scanner.current - scanner.start > 1)
            switch (buf[scanner.start + 1]) {
                'h' => checkKeyword(scanner, buf, 2, "is", TokenType.THIS),
                'r' => checkKeyword(scanner, buf, 2, "ue", TokenType.TRUE),
                else => TokenType.IDENTIFIER,
            }
        else
            TokenType.IDENTIFIER,
        'v' => checkKeyword(scanner, buf, 1, "ar", TokenType.VAR),
        'w' => checkKeyword(scanner, buf, 1, "hile", TokenType.WHILE),
        else => TokenType.IDENTIFIER,
    };
}

fn checkKeyword(scanner: *Scanner, buf: []const u8, start: u16, rest: []const u8, typ: TokenType) TokenType {
    const begin = buf[scanner.start + start ..];
    if (std.mem.startsWith(u8, begin, rest)) {
        return typ;
    }

    return TokenType.IDENTIFIER;
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var buffer: [32000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const gpa = fba.allocator();

    var file = try std.fs.cwd().openFile("foo.txt", .{});
    defer file.close();

    const file_buffer = try file.readToEndAlloc(gpa, 1024);
    var scanner = Scanner{ .start = 0, .current = 0, .line = 1 };
    while (!isAtEnd(&scanner, file_buffer)) {
        // scanToken
        skipWhitespace(&scanner, file_buffer);
        scanner.start = scanner.current;
        if (isAtEnd(&scanner, file_buffer)) continue;
        var c = advance(&scanner, file_buffer);
        const token = try switch (c) {
            '0'...'9' => number(&scanner, file_buffer),
            '(' => makeToken(&scanner, TokenType.LEFT_PAREN),
            ')' => makeToken(&scanner, TokenType.RIGHT_PAREN),
            '{' => makeToken(&scanner, TokenType.LEFT_BRACE),
            '}' => makeToken(&scanner, TokenType.RIGHT_BRACE),
            ';' => makeToken(&scanner, TokenType.SEMICOLON),
            ',' => makeToken(&scanner, TokenType.COMMA),
            '.' => makeToken(&scanner, TokenType.DOT),
            '-' => makeToken(&scanner, TokenType.MINUS),
            '+' => makeToken(&scanner, TokenType.PLUS),
            '/' => makeToken(&scanner, TokenType.SLASH),
            '*' => makeToken(&scanner, TokenType.STAR),
            '!' => makeToken(&scanner, if (match(&scanner, file_buffer, '=')) TokenType.BANG_EQUAL else TokenType.BANG),
            '=' => makeToken(&scanner, if (match(&scanner, file_buffer, '=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
            '<' => makeToken(&scanner, if (match(&scanner, file_buffer, '=')) TokenType.LESS_EQUAL else TokenType.LESS),
            '>' => makeToken(&scanner, if (match(&scanner, file_buffer, '=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
            '"' => string(&scanner, file_buffer),
            else => if (isAlpha(c))
                identifier(&scanner, file_buffer)
            else
                std.debug.panic("unknown char '{s}', scanner state: start={d} current={d} line={d}", .{ [1]u8{c}, scanner.start, scanner.current, scanner.line }),
        };
        try stdout.print("read token {s}\n", .{@tagName(token.typ)});
    }
    //try stdout.print("scanner current={d} buf.len={d}\n", .{ scanner.current, file_buffer.len });

    var chunks = try demoChunks(gpa);
    try stdout.print("chunks: {d}\n", .{chunks.len});

    var stack: [256]f64 = undefined;
    var stackTop: usize = 0;
    const data = chunks.items(.data);
    for (data, 0..) |op, idx| {
        switch (@enumFromInt(Op, op[0])) {
            .CONSTANT => {
                stackTop += 1;
                stack[stackTop] = chunks.items(.constants)[idx][@as(usize, op[1])];
                try stdout.print("constant: {d}, stored at stackTop={d}\n", .{ stack[stackTop], stackTop });
            },
            .DIVIDE => {
                const b = stack[stackTop - 0];
                const a = stack[stackTop - 1];
                stackTop -= 1;
                stack[stackTop] = a / b;
                try stdout.print("divide: a={d} b={d} res={d}, new reduced stackTop={d} \n", .{ a, b, stack[stackTop], stackTop });
            },
            .NEGATE => {
                const old = stack[stackTop];
                const res = -old;
                try stdout.print("negate: old={d} res={d}, stackTop={d} \n", .{ old, res, stackTop });
                stack[stackTop] = res;
            },
            .RETURN => {
                try stdout.print("return: {d} read from stackTop={d}\n", .{ stack[stackTop], stackTop });
            },
            .ADD => {
                try stdout.print("add: original stackTop={d}\n", .{stackTop});
                const b = stack[stackTop - 0];
                const a = stack[stackTop - 1];
                stackTop -= 1;
                stack[stackTop] = a + b;
                try stdout.print("add: a={d} b={d} res={d}, new reduced stackTop={d}\n", .{ a, b, stack[stackTop], stackTop });
            },
        }
    }

    try bw.flush();
}

test "op length" {
    var chunks = try demoChunks(std.testing.allocator);
    defer chunks.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 7), chunks.len);
}

const Chunk = struct {
    data: []const u8,
    constants: []const f64,
    line: u16,
};

const onetwo: [1]f64 = .{1.2};
const threefour: [1]f64 = .{3.4};
const fivesix: [1]f64 = .{5.6};

const Op = enum(u8) {
    CONSTANT,
    ADD,
    DIVIDE,
    NEGATE,
    RETURN,
};

fn demoChunks(gpa: std.mem.Allocator) !std.MultiArrayList(Chunk) {
    var chunks: std.MultiArrayList(Chunk) = std.MultiArrayList(Chunk){};

    try chunks.append(gpa, Chunk{ .data = &[_]u8{ @intFromEnum(Op.CONSTANT), 0 }, .constants = &onetwo, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = &[_]u8{ @intFromEnum(Op.CONSTANT), 0 }, .constants = &threefour, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = &[_]u8{@intFromEnum(Op.ADD)}, .constants = &[0]f64{}, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = &[_]u8{ @intFromEnum(Op.CONSTANT), 0 }, .constants = &fivesix, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = &[_]u8{@intFromEnum(Op.DIVIDE)}, .constants = undefined, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = &[_]u8{@intFromEnum(Op.NEGATE)}, .constants = undefined, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = &[_]u8{@intFromEnum(Op.RETURN)}, .constants = undefined, .line = 123 });
    return chunks;
}
