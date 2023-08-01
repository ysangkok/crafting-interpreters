const std = @import("std");
const scannerMod = @import("scanner.zig");
const Token = scannerMod.Token;
const Scanner = scannerMod.Scanner;
const TokenType = scannerMod.TokenType;
const scanToken = scannerMod.scanToken;

// chapter 15 compiling expressions

const Parser = struct {
    current: Token,
    previous: Token,
};

fn advanceP(parser: *Parser, scanner: *Scanner) void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanToken(scanner);
        if (parser.current.typ != TokenType.ERROR) {
            break;
        }
        // todo report error
    }
}

fn consumeP(parser: *Parser, scanner: *Scanner, typ: TokenType, message: []const u8) void {
    if (parser.current.typ == typ) {
        advanceP(parser, scanner);
        return;
    }

    @panic(message);
}

const ValueTag = enum {
    number,
    boolean,
    nil,
    string,
};

const Value = union(ValueTag) {
    number: f64,
    boolean: bool,
    nil: void,
    string: []u8,
};

fn printValue(stdout: anytype, val: Value) !void {
    switch (val) {
        .number => |num| try stdout.print("printValue: {d}\n", .{num}),
        .boolean => |boo| try stdout.print("printValue: {}\n", .{boo}),
        .string => |str| try stdout.print("printValue: {s}\n", .{str}),
        .nil => try stdout.print("printValue: nil\n", .{}),
    }
}

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

fn grouping(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    expression(gpa, currentChunk, parser, scanner, buf);
    consumeP(parser, scanner, TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    const operatorType = parser.previous.typ;

    // Compile the operand.
    parsePrecedence(gpa, currentChunk, parser, scanner, buf, getPrecedence(operatorType).next());

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

fn expression(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) void {
    parsePrecedence(gpa, currentChunk, parser, scanner, buf, Prec.ASSIGNMENT);
}

fn declaration(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    if (matchP(TokenType.VAR, parser, scanner)) {
        try varDeclaration(gpa, currentChunk, parser, scanner, buf);
    } else {
        try statement(gpa, currentChunk, parser, scanner, buf);
    }
}

fn varDeclaration(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    var global = try parseVariable(gpa, currentChunk, parser, scanner, buf, "Expect variable name.");

    if (matchP(TokenType.EQUAL, parser, scanner)) {
        expression(gpa, currentChunk, parser, scanner, buf);
    } else {
        try currentChunk.data.append(@intFromEnum(Op.NIL));
    }
    consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after variable declaration.");

    try defineVariable(global, currentChunk);
}

fn parseVariable(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, errorMessage: []const u8) !usize {
    consumeP(parser, scanner, TokenType.IDENTIFIER, errorMessage);
    return try identifierConstant(gpa, parser.previous, currentChunk, buf);
}

fn identifierConstant(gpa: std.mem.Allocator, name: Token, currentChunk: *Chunk, buf: []const u8) !u8 {
    var dst = try gpa.alloc(u8, name.length);
    std.mem.copy(u8, dst, buf[name.start .. name.start + name.length]);
    var val: Value = Value{ .string = dst };
    try currentChunk.constants.append(val);
    return @truncate(u8, currentChunk.constants.items.len - 1);
}

fn defineVariable(global: usize, currentChunk: *Chunk) !void {
    try currentChunk.data.append(@intFromEnum(Op.DEFINE_GLOBAL));
    try currentChunk.data.append(@truncate(u8, global));
}

fn check(typ: TokenType, parser: *Parser) bool {
    return parser.current.typ == typ;
}

fn matchP(typ: TokenType, parser: *Parser, scanner: *Scanner) bool {
    if (!check(typ, parser)) return false;
    advanceP(parser, scanner);
    return true;
}

fn statement(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    if (matchP(TokenType.PRINT, parser, scanner)) {
        //std.debug.print("print\n", .{});
        try printStatement(gpa, currentChunk, parser, scanner, buf);
    } else {
        try expressionStatement(gpa, currentChunk, parser, scanner, buf);
    }
}

fn expressionStatement(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    expression(gpa, currentChunk, parser, scanner, buf);
    consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after expression.");
    try currentChunk.data.append(@intFromEnum(Op.POP));
}

fn printStatement(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    expression(gpa, currentChunk, parser, scanner, buf);
    consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after value.");
    try currentChunk.data.append(@intFromEnum(Op.PRINT));
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

fn binary(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8) !void {
    const operatorType: TokenType = parser.previous.typ;
    parsePrecedence(gpa, currentChunk, parser, scanner, buf, getPrecedence(operatorType).next());

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

fn parsePrecedence(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, precedence: Prec) void {
    advanceP(parser, scanner);
    prefix(gpa, currentChunk, parser, scanner, buf, parser.previous.typ);
    while (@intFromEnum(precedence) <= @intFromEnum(getPrecedence(parser.current.typ))) {
        advanceP(parser, scanner);
        infix(gpa, currentChunk, parser, scanner, buf, parser.previous.typ);
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

fn stringP(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, buf: []const u8) void {
    const dst = gpa.alloc(u8, parser.previous.length - 2) catch {
        @panic("oom");
    };
    std.mem.copy(u8, dst, buf[parser.previous.start + 1 .. parser.previous.start + 1 + parser.previous.length - 2]);
    emitConstant(currentChunk, Value{ .string = dst });
}

// from https://github.com/jwmerrill/zig-lox/blob/main/src/compiler.zig
fn prefix(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, tokenType: TokenType) void {
    switch (tokenType) {
        // Single-character tokens.
        .LEFT_PAREN => grouping(gpa, currentChunk, parser, scanner, buf),
        .MINUS, .BANG => unary(gpa, currentChunk, parser, scanner, buf),
        //.RIGHTPAREN, .LEFTBRACE, .RIGHTBRACE, .COMMA, .DOT => try self.prefixError(),
        //.Plus, .Semicolon, .Slash, .Star => try self.prefixError(),
        .NUMBER => numberP(currentChunk, parser, buf),
        .FALSE, .NIL, .TRUE => literalP(currentChunk, parser),
        .STRING => stringP(gpa, currentChunk, parser, buf),
        .IDENTIFIER => variable(gpa, currentChunk, parser, buf) catch {},
        else => panicToken(tokenType),

        //// One or two character tokens.
        //.Bang => try self.unary(),
        //.Equal, .BangEqual, .EqualEqual, .Greater, .GreaterEqual => try self.prefixError(),
        //.Less, .LessEqual => try self.prefixError(),

        //// Keywords.
        //.Nil, .True, .False => try self.literal(),
        //.This => try self.this(),
        //.Super => try self.super(),
        //.And, .Class, .Else, .For, .Fun, .If, .Or => try self.prefixError(),
        //.Print, .Return, .Var, .While, .Error, .Eof => try self.prefixError(),
    }
}

fn variable(gpa: std.mem.Allocator, chunk: *Chunk, parser: *Parser, buf: []const u8) !void {
    try namedVariable(parser.previous, gpa, chunk, buf);
}

fn namedVariable(name: Token, gpa: std.mem.Allocator, currentChunk: *Chunk, buf: []const u8) !void {
    var arg = try identifierConstant(gpa, name, currentChunk, buf);
    try currentChunk.data.append(@intFromEnum(Op.GET_GLOBAL));
    try currentChunk.data.append(arg);
}

fn panicToken(tokenType: TokenType) noreturn {
    var b: [256]u8 = [1]u8{0} ** 256;
    const b2 = std.fmt.bufPrint(&b, "can't handle: {s}", .{@tagName(tokenType)}) catch {
        @panic("bufPrint failed");
    };
    @panic(b2);
}

fn infix(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, buf: []const u8, tokenType: TokenType) void {
    switch (tokenType) {
        // Single-character tokens.
        .MINUS, .PLUS, .SLASH, .STAR, .BANG_EQUAL, .EQUAL_EQUAL, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL => binary(gpa, currentChunk, parser, scanner, buf) catch {
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

    var gen = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa = gen.allocator();

    var file = try std.fs.cwd().openFile("foo.txt", .{});
    defer file.close();

    const file_buffer = try file.readToEndAlloc(gpa, 1024);

    var scanner = Scanner{ .start = 0, .current = 0, .line = 1, .buf = file_buffer };
    var compilingChunk = Chunk{ .data = std.ArrayList(u8).init(gpa), .constants = std.ArrayList(Value).init(gpa), .line = 1 };
    var parser: Parser = undefined;

    advanceP(&parser, &scanner);
    while (true) {
        if (parser.current.typ == TokenType.EOF) {
            break;
        }
        //try stdout.print("declaration? {}\n", .{parser.current.typ});
        try bw.flush();
        try declaration(gpa, &compilingChunk, &parser, &scanner, file_buffer);
    }

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
    try runChunk(gpa, &compilingChunk, &vm);
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

fn get_string(val: Value) ![]const u8 {
    switch (val) {
        .string => |str| return str,
        else => @panic("not a string"),
    }
}

fn valuesEqual(a: Value, b: Value) bool {
    if (@intFromEnum(a) != @intFromEnum(b)) return false;
    switch (a) {
        .boolean => |boo| return boo == b.boolean,
        .nil => return true,
        .number => |num| return num == b.number,
        .string => |str| return std.mem.eql(u8, str, b.string),
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
        .string => |str| return str,
    }
}

fn runChunk(gpa: std.mem.Allocator, chunk: *Chunk, vm: *VM) !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var globals = std.StringHashMap(Value).init(gpa);

    var lst: Op = undefined;
    var ip: usize = 0;
    while (true) {
        if (ip >= chunk.data.items.len) {
            break;
        }
        const op = @enumFromInt(Op, chunk.data.items[ip]);
        try stdout.print("executing ip={d}\n", .{ip});
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
                try bw.flush();
                const b = vm.stack[vm.stackTop - 0];
                const a = vm.stack[vm.stackTop - 1];
                switch (b) {
                    .number => |bn| {
                        const an = try get_number(a);
                        vm.stackTop -= 1;
                        vm.stack[vm.stackTop] = Value{ .number = an + bn };
                        try stdout.print("add: a={d} b={d} res={s}, new reduced stackTop={d}\n", .{ an, bn, showVal(vm.stack[vm.stackTop]), vm.stackTop });
                    },
                    .string => |bs| {
                        const as = try get_string(a);
                        vm.stackTop -= 1;
                        var new = try concatAndReturnBuffer(gpa, as, bs);
                        vm.stack[vm.stackTop] = Value{ .string = new };
                    },
                    .nil => {
                        @panic("invalid nil for + operator");
                    },
                    .boolean => |_| {
                        @panic("invalid bool for + operator");
                    },
                }
            },
            .PRINT => {
                try stdout.print("executing PRINT\n", .{});
                try printValue(stdout, vm.stack[vm.stackTop]);
                vm.stackTop -= 1;
            },
            .POP => {
                try stdout.print("popping stackTop from {d} to {d}\n", .{ vm.stackTop, vm.stackTop - 1 });
                vm.stackTop -= 1;
            },
            .DEFINE_GLOBAL => {
                const name = try get_string(chunk.constants.items[@as(usize, chunk.data.items[ip])]);
                ip += 1;
                //try stdout.print("defining global {s}\n", .{name});
                //try printValue(stdout, vm.stack[vm.stackTop]);
                try globals.putNoClobber(name, vm.stack[vm.stackTop]);
                vm.stackTop -= 1;
                try stdout.print("defining global {s}, decresed stack from {d} (read from here) to {d}\n", .{ name, vm.stackTop + 1, vm.stackTop });
            },
            .GET_GLOBAL => {
                const constidx = @as(usize, chunk.data.items[ip]);
                ip += 1;
                const name = try get_string(chunk.constants.items[constidx]);
                try stdout.print("getting global using constant idx {d}. It is {s}\n", .{ constidx, name });
                var val = globals.get(name);
                if (val) |v| {
                    vm.stackTop += 1;
                    try stdout.print("setting stack index {d}\n", .{vm.stackTop});
                    vm.stack[vm.stackTop] = v;
                    //try stdout.print("stack elem 0: ", .{});
                    //try printValue(stdout, vm.stack[0]);
                    //try stdout.print("stack elem 1: ", .{});
                    //try printValue(stdout, vm.stack[1]);
                    //try stdout.print("stack elem 2: ", .{});
                    //try printValue(stdout, vm.stack[2]);
                    //try stdout.print("stack elem 3: ", .{});
                    //try printValue(stdout, vm.stack[3]);
                } else {
                    @panic("global not found");
                }
            },
        }
    }
    try bw.flush();
}

fn concatAndReturnBuffer(allocator: std.mem.Allocator, one: []const u8, two: []const u8) ![]u8 {
    var b = try allocator.alloc(u8, one.len + two.len);
    std.mem.copy(u8, b, one);
    std.mem.copy(u8, b[one.len..], two);
    return b;
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
    PRINT,
    POP,
    DEFINE_GLOBAL,
    GET_GLOBAL,
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
