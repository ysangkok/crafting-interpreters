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

fn peekNext(scanner: *Scanner, buf: []const u8) u8 {
    if (scanner.current + 1 >= buf.len) return 0;
    return buf[scanner.current + 1];
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
    while (isDigit(peek(scanner, buf))) {
        _ = advance(scanner, buf);
    }

    // Look for a fractional part.
    if (peek(scanner, buf) == '.' and isDigit(peekNext(scanner, buf))) {
        // Consume the ".".
        _ = advance(scanner, buf);

        while (isDigit(peek(scanner, buf))) {
            _ = advance(scanner, buf);
        }
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

// chapter 15 compiling expressions

const Parser = struct {
    current: Token,
    previous: Token,
};

fn advanceP(parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanToken(scanner, buf);
        if (parser.current.typ != TokenType.ERROR) {
            break;
        }
        // todo report error
    }
}

fn consumeP(parser: *Parser, scanner: *Scanner, buf: []const u8, typ: TokenType, message: []const u8) void {
    if (parser.current.typ == typ) {
        advanceP(parser, scanner, buf);
        return;
    }

    @panic(message);
}

const ValueTag = enum {
    number,
    boolean,
    nil,
};

const Value = union(ValueTag) {
    number: f64,
    boolean: bool,
    nil: void,
};

fn numberP(currentChunk: *Chunk, parser: *Parser, buf: []const u8) void {
    const value = std.fmt.parseFloat(f64, buf[parser.previous.start .. parser.previous.start + parser.previous.length]) catch {
        @panic("invalid");
    };
    emitConstant(currentChunk, Value{ .number = value });
}

fn emitConstant(currentChunk: *Chunk, val: Value) void {
    currentChunk.constants.append(val) catch {
        @panic("couldn't append");
    };
    currentChunk.data.append(@intFromEnum(Op.CONSTANT)) catch {
        @panic("couldn't add constant");
    };
    currentChunk.data.append(@truncate(u8, currentChunk.constants.items.len - 1)) catch {
        @panic("couldn't add length");
    };
}

fn grouping(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    expression(currentChunk, parser, scanner, buf);
    consumeP(parser, scanner, buf, TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    const operatorType = parser.previous.typ;

    // Compile the operand.
    parsePrecedence(currentChunk, parser, scanner, buf, getPrecedence(operatorType).next());

    // Emit the operator instruction.
    switch (operatorType) {
        TokenType.BANG => currentChunk.data.append(@intFromEnum(Op.NOT)) catch {
            @panic("error in bang");
        },
        TokenType.MINUS => currentChunk.data.append(@intFromEnum(Op.NEGATE)) catch {
            @panic("error in minus");
        },
        else => unreachable, // Unreachable.
    }
}

const Prec = enum {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,

    pub fn next(self: Prec) Prec {
        return @enumFromInt(Prec, @intFromEnum(self) + 1);
    }
};

fn expression(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    parsePrecedence(currentChunk, parser, scanner, buf, Prec.ASSIGNMENT);
}

fn getPrecedence(tokenType: TokenType) Prec {
    return switch (tokenType) {
        // Single-character tokens.
        .LEFT_PAREN => .CALL,
        .RIGHT_PAREN, .LEFT_BRACE, .RIGHT_BRACE, .COMMA => .NONE,
        .DOT => .CALL,
        .MINUS, .PLUS => .TERM,
        .SEMICOLON => .NONE,
        .SLASH, .STAR => .FACTOR,

        // One or two character tokens.
        .BANG_EQUAL, .EQUAL_EQUAL => .EQUALITY,
        .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL => .COMPARISON,
        .BANG, .EQUAL => .NONE,

        // Literals.
        .IDENTIFIER, .STRING, .NUMBER => .NONE,

        // Keywords.
        .AND => .AND,
        .OR => .OR,
        .CLASS, .ELSE, .FALSE, .FOR, .FUN, .IF, .NIL => .NONE,
        .PRINT, .RETURN, .SUPER, .THIS, .TRUE, .VAR, .WHILE, .ERROR => .NONE,
        .EOF => .NONE,
    };
}

fn binary(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    const operatorType: TokenType = parser.previous.typ;
    parsePrecedence(currentChunk, parser, scanner, buf, getPrecedence(operatorType).next());

    switch (operatorType) {
        TokenType.BANG_EQUAL => try currentChunk.data.appendSlice(&[_]u8{ @intFromEnum(Op.EQUAL), @intFromEnum(Op.NOT) }),
        TokenType.EQUAL_EQUAL => try currentChunk.data.append(@intFromEnum(Op.EQUAL)),
        TokenType.GREATER => try currentChunk.data.append(@intFromEnum(Op.GREATER)),
        TokenType.GREATER_EQUAL => try currentChunk.data.appendSlice(&[_]u8{ @intFromEnum(Op.LESS), @intFromEnum(Op.NOT) }),
        TokenType.LESS => try currentChunk.data.append(@intFromEnum(Op.LESS)),
        TokenType.LESS_EQUAL => try currentChunk.data.appendSlice(&[_]u8{ @intFromEnum(Op.GREATER), @intFromEnum(Op.NOT) }),
        TokenType.PLUS => try currentChunk.data.append(@intFromEnum(Op.ADD)),
        TokenType.MINUS => try currentChunk.data.append(@intFromEnum(Op.SUBTRACT)),
        TokenType.STAR => try currentChunk.data.append(@intFromEnum(Op.MULTIPLY)),
        TokenType.SLASH => try currentChunk.data.append(@intFromEnum(Op.DIVIDE)),
        else => unreachable,
    }
}

fn parsePrecedence(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, precedence: Prec) void {
    advanceP(parser, scanner, buf);
    prefix(currentChunk, parser, scanner, buf, parser.previous.typ);
    while (@intFromEnum(precedence) <= @intFromEnum(getPrecedence(parser.current.typ))) {
        advanceP(parser, scanner, buf);
        infix(currentChunk, parser, scanner, buf, parser.previous.typ);
    }
}

fn literalP(currentChunk: *Chunk, parser: *Parser) void {
    switch (parser.previous.typ) {
        .FALSE => currentChunk.data.append(@intFromEnum(Op.FALSE)) catch {
            @panic("FALSE");
        },
        .NIL => currentChunk.data.append(@intFromEnum(Op.NIL)) catch {
            @panic("NIL");
        },
        .TRUE => currentChunk.data.append(@intFromEnum(Op.TRUE)) catch {
            @panic("TRUE");
        },
        else => unreachable,
    }
}

// from https://github.com/jwmerrill/zig-lox/blob/main/src/compiler.zig
fn prefix(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, tokenType: TokenType) void {
    switch (tokenType) {
        // Single-character tokens.
        .LEFT_PAREN => grouping(currentChunk, parser, scanner, buf),
        .MINUS, .BANG => unary(currentChunk, parser, scanner, buf),
        //.RIGHTPAREN, .LEFTBRACE, .RIGHTBRACE, .COMMA, .DOT => try self.prefixError(),
        //.Plus, .Semicolon, .Slash, .Star => try self.prefixError(),
        .NUMBER => numberP(currentChunk, parser, buf),
        .FALSE, .NIL, .TRUE => literalP(currentChunk, parser),
        else => panicToken(tokenType),

        //// One or two character tokens.
        //.Bang => try self.unary(),
        //.Equal, .BangEqual, .EqualEqual, .Greater, .GreaterEqual => try self.prefixError(),
        //.Less, .LessEqual => try self.prefixError(),

        //// Literals.
        //.Identifier => try self.variable(canAssign),
        //.String => try self.string(),

        //// Keywords.
        //.Nil, .True, .False => try self.literal(),
        //.This => try self.this(),
        //.Super => try self.super(),
        //.And, .Class, .Else, .For, .Fun, .If, .Or => try self.prefixError(),
        //.Print, .Return, .Var, .While, .Error, .Eof => try self.prefixError(),
    }
}

fn panicToken(tokenType: TokenType) noreturn {
    var b: [256]u8 = undefined;
    const b2 = std.fmt.bufPrint(&b, "can't handle: {s}", .{@tagName(tokenType)}) catch {
        @panic("bufPrint failed");
    };
    @panic(b2);
}

fn infix(currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, tokenType: TokenType) void {
    switch (tokenType) {
        // Single-character tokens.
        .MINUS, .PLUS, .SLASH, .STAR, .BANG_EQUAL, .EQUAL_EQUAL, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL => binary(currentChunk, parser, scanner, buf) catch {
            @panic("binary failed");
        },
        else => panicToken(tokenType),
        //.LeftParen => try self.call(),
        //.Dot => try self.dot(canAssign),
        //.RightParen, .LeftBrace, .RightBrace, .Comma, .Semicolon => try self.infixError(),

        //// One or two character tokens.
        //.BangEqual, .EqualEqual, .Greater, .GreaterEqual => try self.binary(),
        //.Less, .LessEqual => try self.binary(),

        //.Bang, .Equal => try self.infixError(),

        //// Literals.
        //.Identifier, .String, .Number => try self.infixError(),

        //// Keywords.
        //.And => try self.and_(),
        //.Or => try self.or_(),
        //.Class, .Else, .False, .For, .Fun, .If, .Nil => try self.infixError(),
        //.Print, .Return, .Super, .This, .True, .Var, .While, .Error, .Eof => try self.infixError(),
    }
}

fn scanToken(scanner: *Scanner, file_buffer: []const u8) Token {
    // scanToken
    skipWhitespace(scanner, file_buffer);
    scanner.start = scanner.current;
    if (isAtEnd(scanner, file_buffer)) return try makeToken(scanner, TokenType.EOF);
    var c = advance(scanner, file_buffer);
    const token = try switch (c) {
        '0'...'9' => number(scanner, file_buffer),
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
        '!' => makeToken(scanner, if (match(scanner, file_buffer, '=')) TokenType.BANG_EQUAL else TokenType.BANG),
        '=' => makeToken(scanner, if (match(scanner, file_buffer, '=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
        '<' => makeToken(scanner, if (match(scanner, file_buffer, '=')) TokenType.LESS_EQUAL else TokenType.LESS),
        '>' => makeToken(scanner, if (match(scanner, file_buffer, '=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
        '"' => string(scanner, file_buffer),
        else => if (isAlpha(c))
            identifier(scanner, file_buffer)
        else
            std.debug.panic("unknown char '{s}', scanner state: start={d} current={d} line={d}", .{ [1]u8{c}, scanner.start, scanner.current, scanner.line }),
    };
    return token;
}

fn disassembleChunk(chunks: *std.MultiArrayList(Chunk)) !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    const data = chunks.items(.data);
    for (data, 0..) |op, idx| {
        const openum = @enumFromInt(Op, op.items[0]);
        try stdout.print("processing idx idx={d} data.len={d} {s} op.items.len={d}\n", .{ idx, data.len, @tagName(openum), op.items.len });
        try bw.flush();
    }
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    //const stdout = bw.writer();

    var buffer: [32000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const gpa = fba.allocator();

    var file = try std.fs.cwd().openFile("foo.txt", .{});
    defer file.close();

    const file_buffer = try file.readToEndAlloc(gpa, 1024);

    var scanner = Scanner{ .start = 0, .current = 0, .line = 1 };
    var compilingChunk = Chunk{ .data = std.ArrayList(u8).init(gpa), .constants = std.ArrayList(Value).init(gpa), .line = 1 };
    var parser: Parser = undefined;
    advanceP(&parser, &scanner, file_buffer);
    expression(&compilingChunk, &parser, &scanner, file_buffer);
    try compilingChunk.data.append(@intFromEnum(Op.RETURN));
    consumeP(&parser, &scanner, file_buffer, TokenType.EOF, "Expect end of expression.");

    //while (!isAtEnd(&scanner, file_buffer)) {
    //    const token = scanToken(&scanner, file_buffer);
    //    try stdout.print("read token {s}\n", .{@tagName(token.typ)});
    //}
    //try stdout.print("scanner current={d} buf.len={d}\n", .{ scanner.current, file_buffer.len });

    {
        var chunks = std.MultiArrayList(Chunk){};
        try chunks.append(gpa, compilingChunk);
        //try stdout.print("chunks: {d}\n", .{chunks.len});
        try disassembleChunk(&chunks);
    }
    var vm = VM{
        .stack = undefined,
        .stackTop = 0,
    };
    try runChunk(&compilingChunk, &vm);
    vm.stackTop = 0;
    std.os.exit(0);

    //var chunks = try demoChunks(gpa);

    //const data = chunks.items(.data);
    //for (data, 0..) |op, idx| {
    //    const openum = @enumFromInt(Op, op.items[0]);
    //    //try stdout.print("processing idx idx={d} data.len={d} {s} op.items.len={d}\n", .{ idx, data.len, @tagName(openum), op.items.len });
    //    //try bw.flush();
    //    switch (openum) {
    //        .CONSTANT => {
    //            const chunkConstants = chunks.items(.constants)[idx].items;
    //            vm.stackTop += 1;
    //            //try stdout.print("{d} {d}\n", .{ op.items[0], op.items[1] });
    //            //try bw.flush();
    //            vm.stack[vm.stackTop] = chunkConstants[@as(usize, op.items[1])];
    //            try stdout.print("constant: {d}, stored at stackTop={d}\n", .{ vm.stack[vm.stackTop], vm.stackTop });
    //        },
    //        .MULTIPLY => {
    //            const b = vm.stack[vm.stackTop - 0];
    //            const a = vm.stack[vm.stackTop - 1];
    //            vm.stackTop -= 1;
    //            vm.stack[vm.stackTop] = a * b;
    //            try stdout.print("mult: a={d} b={d} res={d}, new reduced stackTop={d} \n", .{ a, b, vm.stack[vm.stackTop], vm.stackTop });
    //        },
    //        .DIVIDE => {
    //            const b = vm.stack[vm.stackTop - 0];
    //            const a = vm.stack[vm.stackTop - 1];
    //            vm.stackTop -= 1;
    //            vm.stack[vm.stackTop] = a / b;
    //            try stdout.print("divide: a={d} b={d} res={d}, new reduced stackTop={d} \n", .{ a, b, vm.stack[vm.stackTop], vm.stackTop });
    //        },
    //        .NEGATE => {
    //            const old = vm.stack[vm.stackTop];
    //            const res = -old;
    //            try stdout.print("negate: old={d} res={d}, stackTop={d} \n", .{ old, res, vm.stackTop });
    //            vm.stack[vm.stackTop] = res;
    //        },
    //        .RETURN => {
    //            try stdout.print("return: {d} read from stackTop={d}\n", .{ vm.stack[vm.stackTop], vm.stackTop });
    //        },
    //        .SUBTRACT => {
    //            try stdout.print("sub: original stackTop={d}\n", .{vm.stackTop});
    //            const b = vm.stack[vm.stackTop - 0];
    //            const a = vm.stack[vm.stackTop - 1];
    //            vm.stackTop -= 1;
    //            vm.stack[vm.stackTop] = a - b;
    //            try stdout.print("sub: a={d} b={d} res={d}, new reduced stackTop={d}\n", .{ a, b, vm.stack[vm.stackTop], vm.stackTop });
    //        },
    //        .ADD => {
    //            try stdout.print("add: original stackTop={d}\n", .{vm.stackTop});
    //            const b = vm.stack[vm.stackTop - 0];
    //            const a = vm.stack[vm.stackTop - 1];
    //            vm.stackTop -= 1;
    //            vm.stack[vm.stackTop] = a + b;
    //            try stdout.print("add: a={d} b={d} res={d}, new reduced stackTop={d}\n", .{ a, b, vm.stack[vm.stackTop], vm.stackTop });
    //        },
    //    }
    //}

    try bw.flush();
}

//fn execOp(op: Op, vm: *VM) !void {
//}

const VM = struct {
    stackTop: usize,
    stack: [256]Value,
};

fn get_number(val: Value) !f64 {
    switch (val) {
        .number => |num| return num,
        else => @panic("not a number"),
    }
}

fn get_boolean(val: Value) !bool {
    switch (val) {
        .boolean => |boo| return boo,
        else => @panic("not a boolean"),
    }
}

fn valuesEqual(a: Value, b: Value) bool {
    if (@intFromEnum(a) != @intFromEnum(b)) return false;
    switch (a) {
        .boolean => |boo| return boo == b.boolean,
        .nil => return true,
        .number => |num| return num == b.number,
    }
}

fn isFalsey(value: Value) bool {
    switch (value) {
        .nil => return true,
        .boolean => |boo| return !boo,
        else => return false,
    }
}

fn showVal(val: Value) []const u8 {
    switch (val) {
        .nil => return "nil",
        .boolean => |b| if (b) {
            return "true";
        } else {
            return "false";
        },
        .number => |n| {
            var a: [8]u8 = .{ 0, 0, 0, 0, 0, 0, 0, 0 };
            _ = std.fmt.bufPrint(&a, "{d}", .{n}) catch {
                @panic("lol");
            };
            return &a;
        },
    }
}

fn runChunk(chunk: *Chunk, vm: *VM) !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var lst: Op = undefined;
    var ip: usize = 0;
    while (true) {
        if (ip >= chunk.data.items.len) {
            break;
        }
        const op = @enumFromInt(Op, chunk.data.items[ip]);
        ip += 1;
        lst = op;
        switch (op) {
            .NIL => {
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = Value{ .nil = {} };
            },
            .TRUE => {
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = Value{ .boolean = true };
            },
            .FALSE => {
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = Value{ .boolean = false };
            },
            .EQUAL => {
                const b = vm.stack[vm.stackTop - 0];
                const a = vm.stack[vm.stackTop - 1];
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .boolean = valuesEqual(a, b) };
            },
            .GREATER => {
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .boolean = a > b };
            },
            .LESS => {
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .boolean = a < b };
            },
            .NOT => {
                const b = vm.stack[vm.stackTop];
                vm.stack[vm.stackTop] = Value{ .boolean = isFalsey(b) };
            },
            .CONSTANT => {
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = chunk.constants.items[@as(usize, chunk.data.items[ip])];
                ip += 1;
                try stdout.print("constant: {s}, stored at stackTop={d}\n", .{ showVal(vm.stack[vm.stackTop]), vm.stackTop });
            },
            .MULTIPLY => {
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a * b };
                try stdout.print("mult: a={d} b={d} res={s}, new reduced stackTop={d} \n", .{ a, b, showVal(vm.stack[vm.stackTop]), vm.stackTop });
            },
            .DIVIDE => {
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a / b };
                try stdout.print("divide: a={d} b={d} res={s}, new reduced stackTop={d} \n", .{ a, b, showVal(vm.stack[vm.stackTop]), vm.stackTop });
            },
            .NEGATE => {
                const old = try get_number(vm.stack[vm.stackTop]);
                const res = -old;
                try stdout.print("negate: old={d} res={d}, stackTop={d} \n", .{ old, res, vm.stackTop });
                vm.stack[vm.stackTop] = Value{ .number = res };
            },
            .RETURN => {
                try stdout.print("return: {s} read from stackTop={d}\n", .{ showVal(vm.stack[vm.stackTop]), vm.stackTop });
            },
            .SUBTRACT => {
                try stdout.print("sub: original stackTop={d}\n", .{vm.stackTop});
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a - b };
                try stdout.print("sub: a={d} b={d} res={d}, new reduced stackTop={d}\n", .{ a, b, showVal(vm.stack[vm.stackTop]), vm.stackTop });
            },
            .ADD => {
                try stdout.print("add: original stackTop={d}\n", .{vm.stackTop});
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a + b };
                try stdout.print("add: a={d} b={d} res={s}, new reduced stackTop={d}\n", .{ a, b, showVal(vm.stack[vm.stackTop]), vm.stackTop });
            },
        }
    }
    if (lst != .RETURN) {
        @panic("expected return");
    }
    try bw.flush();
}

test "op length" {
    var chunks = try demoChunks(std.testing.allocator);
    defer chunks.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 7), chunks.len);
}

const Chunk = struct {
    data: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    line: u16,
};

var onetwo: [1]f64 = .{1.2};
var threefour: [1]f64 = .{3.4};
var fivesix: [1]f64 = .{5.6};

const Op = enum(u8) {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    NEGATE,
    RETURN,
};

fn demoChunks(gpa: std.mem.Allocator) !std.MultiArrayList(Chunk) {
    var chunks: std.MultiArrayList(Chunk) = std.MultiArrayList(Chunk){};

    const const0 = [_]u8{ @intFromEnum(Op.CONSTANT), 0 };
    const add = [_]u8{@intFromEnum(Op.ADD)};
    const div = [_]u8{@intFromEnum(Op.DIVIDE)};
    const neg = [_]u8{@intFromEnum(Op.NEGATE)};
    const ret = [_]u8{@intFromEnum(Op.RETURN)};

    var data0 = std.ArrayList(u8).init(gpa);
    try data0.appendSlice(&const0);
    var data1 = std.ArrayList(u8).init(gpa);
    try data1.appendSlice(&const0);
    var data2 = std.ArrayList(u8).init(gpa);
    try data2.appendSlice(&add);
    var data3 = std.ArrayList(u8).init(gpa);
    try data3.appendSlice(&const0);
    var data4 = std.ArrayList(u8).init(gpa);
    try data4.appendSlice(&div);
    var data5 = std.ArrayList(u8).init(gpa);
    try data5.appendSlice(&neg);
    var data6 = std.ArrayList(u8).init(gpa);
    try data6.appendSlice(&ret);

    var constants0 = std.ArrayList(f64).init(gpa);
    try constants0.appendSlice(&onetwo);
    var constants1 = std.ArrayList(f64).init(gpa);
    try constants1.appendSlice(&threefour);
    var constants2 = std.ArrayList(f64).init(gpa);
    try constants2.appendSlice(&fivesix);

    try chunks.append(gpa, Chunk{ .data = data0, .constants = constants0, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = data1, .constants = constants1, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = data2, .constants = undefined, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = data3, .constants = constants2, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = data4, .constants = undefined, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = data5, .constants = undefined, .line = 123 });
    try chunks.append(gpa, Chunk{ .data = data6, .constants = undefined, .line = 123 });
    return chunks;
}
