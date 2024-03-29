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

    std.debug.print("mismatching, consumeP crashing: {s} {s}\n", .{ @tagName(parser.current.typ), @tagName(typ) });
    @panic(message);
}

const ValueTag = enum {
    number,
    boolean,
    nil,
    string,
    function,
    closure,
    upvalue,
};

const Function = struct {
    arity: u8,
    upvalueCount: u8,
    chunk: Chunk,
    name: []const u8,
};

const Value = union(ValueTag) {
    number: f64,
    boolean: bool,
    nil: void,
    string: []u8,
    function: *Function,
    closure: *Closure,
    upvalue: *ObjUpvalue,
};

const ObjUpvalue = struct {
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
};

const Closure = struct {
    obj: Value,
    upvalues: std.ArrayList(?*ObjUpvalue),
    function: *Function,
};

const CallFrame = struct {
    closure: *Closure,
    ip: usize,
    slotsOffset: usize,
};

const red = "\x1b[31m";
const reset_sequence = "\x1b[0m";

fn newUpvalue(gpa: std.mem.Allocator, value: *Value) !*ObjUpvalue {
    const up: *ObjUpvalue = try gpa.create(ObjUpvalue);
    up.location = value;
    up.closed = Value{ .nil = void{} };
    up.next = null;
    return up;
}

fn newClosure(gpa: std.mem.Allocator, fun: *Function) !*Closure {
    const closure = try gpa.create(Closure);
    closure.function = fun;
    closure.upvalues = std.ArrayList(?*ObjUpvalue).init(gpa);
    for (0..fun.upvalueCount) |_| try closure.upvalues.append(null);
    return closure;
}

fn printValue(stdout: anytype, val: Value) !void {
    switch (val) {
        .closure => |clj| try stdout.print("printValue: {s}<closure {s}>{s}\n", .{ red, clj.function.name, reset_sequence }),
        .number => |num| try stdout.print("printValue: {s}{d}{s}\n", .{ red, num, reset_sequence }),
        .boolean => |boo| try stdout.print("printValue: {s}{}{s}\n", .{ red, boo, reset_sequence }),
        .string => |str| try stdout.print("printValue: {s}{s}{s}\n", .{ red, str, reset_sequence }),
        .nil => try stdout.print("printValue: {s}nil{s}\n", .{ red, reset_sequence }),
        .function => |_| try stdout.print("printValue: {s}function{s}\n", .{ red, reset_sequence }),
        .upvalue => |_| try stdout.print("printValue: {s}upvalue{s}\n", .{ red, reset_sequence }),
    }
}

fn numberP(currentChunk: *Chunk, parser: *Parser, buf: []const u8) void {
    const value = std.fmt.parseFloat(f64, buf[parser.previous.start .. parser.previous.start + parser.previous.length]) catch {
        @panic("invalid");
    };
    emitConstant(currentChunk, Value{ .number = value });
}

fn emitJump(instruction: Op, currentChunk: *Chunk) !u16 {
    try currentChunk.data.append(@intFromEnum(instruction));
    try currentChunk.data.append(0xff);
    try currentChunk.data.append(0xff);
    if (currentChunk.data.items.len >= 0xffff)
        @panic("chunk too long");
    return @truncate(u16, currentChunk.data.items.len - 2);
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

fn emitLoop(loopStart: u16, currentChunk: *Chunk) !void {
    try currentChunk.data.append(@intFromEnum(Op.LOOP));

    if (currentChunk.data.items.len > 0xffff)
        @panic("chunk too big");

    var len = @truncate(u16, currentChunk.data.items.len);
    if (loopStart > len or loopStart >= 0xfffd) {
        @panic("loop start too big");
    }
    const offset: u16 = len - loopStart + 2;

    try currentChunk.data.append(@truncate(u8, (offset >> 8) & 0xff));
    try currentChunk.data.append(@truncate(u8, offset & 0xff));
}

fn patchJump(offset: u16, currentChunk: *Chunk) void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump = @intCast(i64, currentChunk.data.items.len) - @intCast(i64, offset) - 2;

    if (jump < 0) {
        @panic("offset pointed too far back");
    }
    if (jump > 0xffff) {
        @panic("Too much code to jump over.");
    }

    currentChunk.data.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
    currentChunk.data.items[offset + 1] = @intCast(u8, jump & 0xff);
}

fn grouping(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) void {
    expression(gpa, parser, scanner, current);
    consumeP(parser, scanner, TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) void {
    const operatorType = parser.previous.typ;

    // Compile the operand.
    parsePrecedence(gpa, current.currentChunk(), parser, scanner, current, getPrecedence(operatorType).next());

    // Emit the operator instruction.
    switch (operatorType) {
        TokenType.BANG => current.currentChunk().data.append(@intFromEnum(Op.NOT)) catch {
            @panic("error in bang");
        },
        TokenType.MINUS => current.currentChunk().data.append(@intFromEnum(Op.NEGATE)) catch {
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

fn expression(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) void {
    parsePrecedence(gpa, current.currentChunk(), parser, scanner, current, Prec.ASSIGNMENT);
}

fn funDeclaration(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, compiler: *Compiler) !void {
    const global = try parseVariable(gpa, parser, scanner, compiler, "Expect function name.");
    compiler.markInitialized();
    try function(gpa, FunctionType.function, parser, scanner, compiler);
    try defineVariable(global, compiler);
}

fn declaration(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, compiler: *Compiler) !void {
    if (matchP(TokenType.FUN, parser, scanner)) {
        try funDeclaration(gpa, parser, scanner, compiler);
    } else if (matchP(TokenType.VAR, parser, scanner)) {
        try varDeclaration(gpa, parser, scanner, compiler);
    } else {
        try statement(gpa, parser, scanner, compiler);
    }
}

fn varDeclaration(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    var global = try parseVariable(gpa, parser, scanner, current, "Expect variable name.");

    if (matchP(TokenType.EQUAL, parser, scanner)) {
        expression(gpa, parser, scanner, current);
    } else {
        try current.currentChunk().data.append(@intFromEnum(Op.NIL));
    }
    consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after variable declaration.");

    std.debug.print("defining variable {d}\n", .{global});
    try defineVariable(global, current);
}

fn parseVariable(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler, errorMessage: []const u8) !usize {
    consumeP(parser, scanner, TokenType.IDENTIFIER, errorMessage);

    try declareVariable(scanner, parser, current);
    if (current.scopeDepth > 0) return 0;

    return try identifierConstant(gpa, parser.previous, current.currentChunk(), scanner.buf);
}

fn identifierConstant(gpa: std.mem.Allocator, name: Token, currentChunk: *Chunk, buf: []const u8) !u8 {
    var dst = try gpa.alloc(u8, name.length);
    std.mem.copy(u8, dst, buf[name.start .. name.start + name.length]);
    var val: Value = Value{ .string = dst };
    try currentChunk.constants.append(val);
    return @truncate(u8, currentChunk.constants.items.len - 1);
}

fn identifiersEqual(scanner: *Scanner, a: *const Token, b: *const Token) bool {
    if (a.length != b.length) return false;
    const as = scanner.buf[a.start .. a.start + a.length];
    const bs = scanner.buf[b.start .. b.start + b.length];
    std.debug.print("comparing '{s}' '{s}'\n", .{ as, bs });
    return std.mem.eql(u8, as, bs);
}

fn addLocal(current: *Compiler, name: Token) !void {
    try current.locals.append(Local{ .name = name, .depth = -1, .isCaptured = false });
}

fn declareVariable(scanner: *Scanner, parser: *Parser, current: *Compiler) !void {
    if (current.scopeDepth == 0) return;

    var name: Token = parser.previous;
    var i: i16 = @intCast(i16, current.locals.items.len);
    i -= 1; // initially we point to the last elem
    while (i >= 0) : (i -= 1) {
        var local = &current.locals.items[@intCast(usize, i)];
        if (local.depth != -1 and local.depth < current.scopeDepth) {
            break;
        }

        if (identifiersEqual(scanner, &name, &local.name)) {
            std.debug.print("Existing local (with index {d}) has depth {d}, we are on scopeDepth {d}\n", .{ i, local.depth, current.scopeDepth });
            @panic("Already a variable with this name in this scope.");
        }
    }
    try addLocal(current, name);
}

fn defineVariable(global: usize, current: *Compiler) !void {
    if (current.scopeDepth > 0) {
        current.markInitialized();
        return;
    }
    try current.currentChunk().data.append(@intFromEnum(Op.DEFINE_GLOBAL));
    try current.currentChunk().data.append(@truncate(u8, global));
}

fn and_(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    const endJump = try emitJump(Op.JUMP_IF_FALSE, currentChunk);

    try currentChunk.data.append(@intFromEnum(Op.POP));
    parsePrecedence(gpa, currentChunk, parser, scanner, current, Prec.AND);

    patchJump(endJump, currentChunk);
}

fn or_(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    const elseJump = try emitJump(Op.JUMP_IF_FALSE, currentChunk);
    const endJump = try emitJump(Op.JUMP, currentChunk);

    patchJump(elseJump, currentChunk);
    try currentChunk.data.append(@intFromEnum(Op.POP));

    parsePrecedence(gpa, currentChunk, parser, scanner, current, Prec.OR);
    patchJump(endJump, currentChunk);
}

fn check(typ: TokenType, parser: *Parser) bool {
    return parser.current.typ == typ;
}

fn matchP(typ: TokenType, parser: *Parser, scanner: *Scanner) bool {
    if (!check(typ, parser)) return false;
    advanceP(parser, scanner);
    return true;
}

fn statement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, compiler: *Compiler) anyerror!void {
    if (matchP(TokenType.PRINT, parser, scanner)) {
        //std.debug.print("print\n", .{});
        try printStatement(gpa, parser, scanner, compiler);
    } else if (matchP(TokenType.FOR, parser, scanner)) {
        try forStatement(gpa, parser, scanner, compiler);
    } else if (matchP(TokenType.IF, parser, scanner)) {
        try ifStatement(gpa, parser, scanner, compiler);
    } else if (matchP(TokenType.RETURN, parser, scanner)) {
        try returnStatement(gpa, parser, scanner, compiler);
    } else if (matchP(TokenType.WHILE, parser, scanner)) {
        try whileStatement(gpa, parser, scanner, compiler);
    } else if (matchP(TokenType.LEFT_BRACE, parser, scanner)) {
        compiler.beginScope();
        block(gpa, parser, scanner, compiler);
        try compiler.endScope();
    } else {
        try expressionStatement(gpa, parser, scanner, compiler);
    }
}

fn block(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, compiler: *Compiler) void {
    while (!check(TokenType.RIGHT_BRACE, parser) and !check(TokenType.EOF, parser)) {
        declaration(gpa, parser, scanner, compiler) catch {
            @panic("exception in declaration");
        };
    }

    consumeP(parser, scanner, TokenType.RIGHT_BRACE, "Expect '}' after block.");
}

fn function(gpa: std.mem.Allocator, funtyp: FunctionType, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    const chk = Chunk{ .data = std.ArrayList(u8).init(gpa), .constants = std.ArrayList(Value).init(gpa), .line = 0 };
    var fun = try gpa.create(Function);
    fun.* = Function{ .arity = 0, .upvalueCount = 0, .chunk = chk, .name = "" };
    var compiler = try gpa.create(Compiler);
    compiler.* = Compiler{ .upvalues = undefined, .enclosing = current, .function_type = funtyp, .function = fun, .locals = std.ArrayList(Local).init(gpa), .scopeDepth = 0 };

    if (funtyp != FunctionType.script) {
        const dst = try gpa.alloc(u8, parser.previous.length);
        std.mem.copy(u8, dst, scanner.buf[parser.previous.start .. parser.previous.start + parser.previous.length]);
        compiler.function.name = dst;
    }

    var local = Local{ .depth = 0, .name = Token{ .line = 0, .typ = TokenType.ERROR, .start = 0, .length = 0 }, .isCaptured = false };
    try compiler.locals.append(local);

    compiler.beginScope();
    consumeP(parser, scanner, TokenType.LEFT_PAREN, "Expect '(' after fun name.");
    if (!check(TokenType.RIGHT_PAREN, parser)) {
        while (true) {
            // using compiler instead of current since initCompiler in chapter 24
            // overwrites current with the passed in compiler
            // so by this point, current would be the compiler, not the old one
            compiler.function.arity += 1;
            if (compiler.function.arity > 255) {
                @panic("Can't have more than 255 parameters.");
            }
            const constant = try parseVariable(gpa, parser, scanner, compiler, "Expect parameter name.");
            try defineVariable(constant, compiler);
            if (!matchP(TokenType.COMMA, parser, scanner)) break;
        }
    }
    consumeP(parser, scanner, TokenType.RIGHT_PAREN, "Expect ')' after fun params.");
    consumeP(parser, scanner, TokenType.LEFT_BRACE, "Expect '{' before fun body.");
    block(gpa, parser, scanner, compiler);

    const buf = try gpa.alloc(u8, 2 * fun.upvalueCount);
    for (0..fun.upvalueCount) |idx| {
        const upvalue: *Upvalue = &compiler.upvalues[idx];
        std.debug.print("wrote upvalue {*}: {d}\n", .{ upvalue, upvalue.index });
        if (upvalue.isLocal) {
            buf[idx * 2] = 1;
        } else {
            buf[idx * 2] = 0;
        }
        buf[idx * 2 + 1] = upvalue.index;
    }
    const finished = try compiler.endCompiler(); // this overwrites compiler's contents with the enclosing compiler
    const chunk = compiler.currentChunk();
    try chunk.data.append(@intFromEnum(Op.CLOSURE));
    try chunk.constants.append(Value{ .function = finished });
    try chunk.data.append(@truncate(u8, chunk.constants.items.len) - 1);

    try chunk.data.appendSlice(buf);
}

fn expressionStatement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    expression(gpa, parser, scanner, current);
    consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after expression.");
    try current.currentChunk().data.append(@intFromEnum(Op.POP));
}

fn forStatement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, compiler: *Compiler) !void {
    compiler.beginScope();
    consumeP(parser, scanner, TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
    if (matchP(TokenType.SEMICOLON, parser, scanner)) {
        // No initializer.
    } else if (matchP(TokenType.VAR, parser, scanner)) {
        try varDeclaration(gpa, parser, scanner, compiler);
    } else {
        try expressionStatement(gpa, parser, scanner, compiler);
    }

    if (compiler.currentChunk().data.items.len >= 0xfff0)
        @panic("chunk too big");
    var loopStart = @truncate(u16, compiler.currentChunk().data.items.len);
    var exitJump: ?u16 = null; // -1 in original source
    if (!matchP(TokenType.SEMICOLON, parser, scanner)) {
        expression(gpa, parser, scanner, compiler);
        consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = try emitJump(Op.JUMP_IF_FALSE, compiler.currentChunk());
        try compiler.currentChunk().data.append(@intFromEnum(Op.POP)); // Condition.
    }

    if (!matchP(TokenType.RIGHT_PAREN, parser, scanner)) {
        const bodyJump = try emitJump(Op.JUMP, compiler.currentChunk());
        const incrementStart = @truncate(u16, compiler.currentChunk().data.items.len);
        expression(gpa, parser, scanner, compiler);
        try compiler.currentChunk().data.append(@intFromEnum(Op.POP));
        consumeP(parser, scanner, TokenType.RIGHT_PAREN, "Expect ')' after for clauses.");

        try emitLoop(loopStart, compiler.currentChunk());
        loopStart = incrementStart;
        patchJump(bodyJump, compiler.currentChunk());
    }

    try statement(gpa, parser, scanner, compiler);
    try emitLoop(loopStart, compiler.currentChunk());

    if (exitJump) |nonNullExitJump| {
        patchJump(nonNullExitJump, compiler.currentChunk());
        try compiler.currentChunk().data.append(@intFromEnum(Op.POP)); // Condition.
    }

    try compiler.endScope();
}

fn ifStatement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    consumeP(parser, scanner, TokenType.LEFT_PAREN, "expect '('");
    expression(gpa, parser, scanner, current);
    consumeP(parser, scanner, TokenType.RIGHT_PAREN, "expect ')'");

    const thenJump = try emitJump(Op.JUMP_IF_FALSE, current.currentChunk());
    try current.currentChunk().data.append(@intFromEnum(Op.POP));
    try statement(gpa, parser, scanner, current);

    const elseJump = try emitJump(Op.JUMP, current.currentChunk());

    patchJump(thenJump, current.currentChunk());
    try current.currentChunk().data.append(@intFromEnum(Op.POP));

    if (matchP(TokenType.ELSE, parser, scanner)) {
        try statement(gpa, parser, scanner, current);
    }
    patchJump(elseJump, current.currentChunk());
}

fn printStatement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    expression(gpa, parser, scanner, current);
    consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after value.");
    try current.currentChunk().data.append(@intFromEnum(Op.PRINT));
}

fn returnStatement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    if (current.function_type == FunctionType.script) {
        @panic("can't return from top-level");
    }
    if (matchP(TokenType.SEMICOLON, parser, scanner)) {
        try current.currentChunk().data.append(@intFromEnum(Op.NIL));
        try current.currentChunk().data.append(@intFromEnum(Op.RETURN));
    } else {
        expression(gpa, parser, scanner, current);
        consumeP(parser, scanner, TokenType.SEMICOLON, "Expect ';' after return value;");
        try current.currentChunk().data.append(@intFromEnum(Op.RETURN));
    }
}

fn whileStatement(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    const loopStart = current.currentChunk().data.items.len;
    consumeP(parser, scanner, TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    expression(gpa, parser, scanner, current);
    consumeP(parser, scanner, TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const exitJump = try emitJump(Op.JUMP_IF_FALSE, current.currentChunk());
    try current.currentChunk().data.append(@intFromEnum(Op.POP));
    try statement(gpa, parser, scanner, current);
    if (loopStart > 0xffff) @panic("too big");
    try emitLoop(@truncate(u16, loopStart), current.currentChunk());

    patchJump(exitJump, current.currentChunk());
    try current.currentChunk().data.append(@intFromEnum(Op.POP));
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

fn binary(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    const operatorType: TokenType = parser.previous.typ;
    parsePrecedence(gpa, currentChunk, parser, scanner, current, getPrecedence(operatorType).next());

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

fn parsePrecedence(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler, precedence: Prec) void {
    advanceP(parser, scanner);
    var canAssign = @intFromEnum(precedence) <= @intFromEnum(Prec.ASSIGNMENT);
    if (canAssign) {
        std.debug.print("canAssign=true\n", .{});
    } else {
        std.debug.print("canAssign=false\n", .{});
    }
    prefix(gpa, currentChunk, parser, scanner, current, parser.previous.typ, canAssign);
    while (@intFromEnum(precedence) <= @intFromEnum(getPrecedence(parser.current.typ))) {
        advanceP(parser, scanner);
        infix(gpa, currentChunk, parser, scanner, current, parser.previous.typ, canAssign);
    }

    if (canAssign and matchP(TokenType.EQUAL, parser, scanner)) {
        @panic("Invalid assignment target");
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
fn prefix(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler, tokenType: TokenType, canAssign: bool) void {
    std.debug.print("prefix {s}\n", .{@tagName(tokenType)});
    switch (tokenType) {
        // Single-character tokens.
        .LEFT_PAREN => grouping(gpa, parser, scanner, current),
        .MINUS, .BANG => unary(gpa, parser, scanner, current),
        //.RIGHTPAREN, .LEFTBRACE, .RIGHTBRACE, .COMMA, .DOT => try self.prefixError(),
        //.Plus, .Semicolon, .Slash, .Star => try self.prefixError(),
        .NUMBER => numberP(currentChunk, parser, scanner.buf),
        .FALSE, .NIL, .TRUE => literalP(currentChunk, parser),
        .STRING => stringP(gpa, currentChunk, parser, scanner.buf),
        .IDENTIFIER => variable(gpa, currentChunk, parser, scanner, current, canAssign) catch {
            @panic("exception in identifier");
        },
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

fn variable(gpa: std.mem.Allocator, chunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler, canAssign: bool) !void {
    try namedVariable(parser.previous, gpa, parser, scanner, chunk, current, canAssign);
}

fn namedVariable(name: Token, gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, currentChunk: *Chunk, current: *Compiler, canAssign: bool) !void {
    var getOp: Op = undefined;
    var setOp: Op = undefined;
    var iarg = resolveLocal(scanner, current, &name);
    var arg: u8 = undefined;
    if (iarg != -1) {
        std.debug.print("namedVariable: local iarg={d}\n", .{iarg});
        arg = @truncate(u8, @bitCast(u16, iarg));
        getOp = Op.GET_LOCAL;
        setOp = Op.SET_LOCAL;
    } else {
        const argu: ?u8 = resolveUpvalue(scanner, current, &name);
        if (argu != null) {
            arg = argu.?;
            getOp = Op.GET_UPVALUE;
            setOp = Op.SET_UPVALUE;
        } else {
            std.debug.print("namedVariable: global\n", .{});
            arg = try identifierConstant(gpa, name, currentChunk, scanner.buf);
            getOp = Op.GET_GLOBAL;
            setOp = Op.SET_GLOBAL;
        }
    }

    if (canAssign and matchP(TokenType.EQUAL, parser, scanner)) {
        std.debug.print("namedVariable: emitting set op {s}\n", .{@tagName(parser.current.typ)});
        expression(gpa, parser, scanner, current);
        try currentChunk.data.append(@intFromEnum(setOp));
        try currentChunk.data.append(arg);
    } else {
        std.debug.print("namedVariable: emitting get op {s}\n", .{@tagName(parser.current.typ)});
        try currentChunk.data.append(@intFromEnum(getOp));
        try currentChunk.data.append(arg);
    }
}

fn resolveLocal(scanner: *Scanner, compiler: *Compiler, name: *const Token) i16 {
    var i = @intCast(i16, compiler.locals.items.len);
    std.debug.print("resolveLocal: amt of locals {d}\n", .{i});
    i -= 1; // initiallly we point to the last element
    while (i >= 0) : (i -= 1) {
        var local = compiler.locals.items[@intCast(usize, i)];
        std.debug.print("resolveLocal: scrutinizing {d} {s}\n", .{ i, @tagName(local.name.typ) });
        if (identifiersEqual(scanner, name, &local.name)) {
            if (local.depth == -1) {
                @panic("Can't read local variable in its own initializer.");
            }
            std.debug.print("resolveLocal: match\n", .{});

            return @intCast(i16, i);
        } else {
            std.debug.print("resolveLocal: no match\n", .{});
        }
    }

    return -1;
}

fn addUpvalue(compiler: *Compiler, index: u8, isLocal: bool) u8 {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const new = Upvalue{ .isLocal = isLocal, .index = index };

    const upvalueCount = compiler.function.upvalueCount;
    for (0..upvalueCount) |idx| {
        const upval = compiler.upvalues[idx];
        if (upval.index == new.index and upval.isLocal == new.isLocal) {
            //if (upval.index == 170) @panic("early return");
            return @truncate(u8, idx);
        }
    }
    //if (index == 170) @panic("one seventy");
    compiler.upvalues[upvalueCount].isLocal = isLocal;
    compiler.upvalues[upvalueCount].index = index;

    compiler.function.upvalueCount += 1;
    if (true) {
        stdout.print("{*} index set to {d}\n", .{ &compiler.upvalues[upvalueCount], index }) catch {};
        bw.flush() catch {};
    }
    return compiler.function.upvalueCount - 1;
}

fn resolveUpvalue(scanner: *Scanner, compiler: *Compiler, name: *const Token) ?u8 {
    if (compiler.enclosing == null) {
        // must be a global
        return null;
    }

    const local = resolveLocal(scanner, compiler.enclosing.?, name);
    if (local != -1) {
        compiler.enclosing.?.locals.items[@intCast(usize, local)].isCaptured = true;
        return addUpvalue(compiler, @intCast(u8, local), true);
    }

    const upvalue = resolveUpvalue(scanner, compiler.enclosing.?, name);
    if (upvalue != null) {
        return addUpvalue(compiler, upvalue.?, false);
    }

    return null;
}

fn panicToken(tokenType: TokenType) noreturn {
    var b: [256]u8 = [1]u8{0} ** 256;
    const b2 = std.fmt.bufPrint(&b, "can't handle: {s}", .{@tagName(tokenType)}) catch {
        @panic("bufPrint failed");
    };
    @panic(b2);
}

fn argumentList(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) u8 {
    var argCount: u8 = 0;
    if (!check(TokenType.RIGHT_PAREN, parser)) {
        while (true) {
            expression(gpa, parser, scanner, current);
            if (argCount == 255) {
                @panic("Can't have more than 255 arguments.");
            }
            argCount += 1;
            if (!matchP(TokenType.COMMA, parser, scanner)) {
                break;
            }
        }
    }

    consumeP(parser, scanner, TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

fn callP(gpa: std.mem.Allocator, parser: *Parser, scanner: *Scanner, current: *Compiler) !void {
    const argCount = argumentList(gpa, parser, scanner, current);
    try current.currentChunk().data.append(@intFromEnum(Op.CALL));
    try current.currentChunk().data.append(argCount);
}

fn infix(gpa: std.mem.Allocator, currentChunk: *Chunk, parser: *Parser, scanner: *Scanner, current: *Compiler, tokenType: TokenType, canAssign: bool) void {
    _ = canAssign;
    switch (tokenType) {
        // Single-character tokens.
        .MINUS, .PLUS, .SLASH, .STAR, .BANG_EQUAL, .EQUAL_EQUAL, .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL => binary(gpa, currentChunk, parser, scanner, current) catch {
            @panic("binary failed");
        },
        .AND => {
            and_(gpa, currentChunk, parser, scanner, current) catch {
                @panic("and failed");
            };
        },
        .OR => {
            or_(gpa, currentChunk, parser, scanner, current) catch {
                @panic("or failed");
            };
        },
        .LEFT_PAREN => {
            callP(gpa, parser, scanner, current) catch {
                @panic("call failed");
            };
        },
        else => panicToken(tokenType),
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

fn disassClosure(opdata: []const u8, constants: []const Value) !usize {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var opidx: usize = 0;
    const constidx = opdata[opidx];
    opidx += 1;
    const fun = try get_function(constants[constidx]);
    try stdout.print("op idx: reading function {s} from constant idx {d}: upvalueCount: {d}\n", .{ fun.name, constidx, fun.upvalueCount });
    for (0..fun.upvalueCount) |_| {
        const isLocal = opdata[opidx];
        opidx += 1;
        const index = opdata[opidx];
        opidx += 1;
        try stdout.print("{d} {d}\n", .{ isLocal, index });
    }
    return opidx;
}

fn disassembleChunk(gpa: std.mem.Allocator, opdata: []const u8, constants: []const Value) !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var opidx: usize = 0;
    while (opidx < opdata.len) : (opidx += 1) {
        const op = opdata[opidx];
        try stdout.print("idx idx={d} op={d} tag={s} len={d}\n", .{ opidx, op, @tagName(@enumFromInt(Op, op)), opdata.len });
        const o = @enumFromInt(Op, op);
        if (o == Op.CONSTANT) {
            const constidx = opdata[opidx + 1];
            try bw.flush();
            try stdout.print("constant {d} {s}\n", .{ constidx, try showVal(gpa, constants[constidx]) });
        }
        switch (o) {
            .GET_LOCAL, .SET_LOCAL, .SET_GLOBAL, .GET_GLOBAL, .CONSTANT, .DEFINE_GLOBAL, .CALL, .GET_UPVALUE, .SET_UPVALUE, .CLOSE_UPVALUE => {
                opidx += 1;
            },
            .CLOSURE => {
                const incr = try disassClosure(opdata[opidx + 1 ..], constants);
                opidx += incr;
            },
            .JUMP, .JUMP_IF_FALSE, .LOOP => {
                opidx += 2;
            },
            .POP, .NIL => {},
            else => {
                try stdout.print("{s}: ", .{@tagName(o)});
                try bw.flush();
                @panic("bad instruction in disassembler");
            },
        }
    }
    try bw.flush();
}

const Local = struct {
    name: Token,
    depth: i16,
    isCaptured: bool,
};

const Upvalue = struct {
    index: u8,
    isLocal: bool,
};

const FunctionType = enum {
    function,
    script,
};

const Compiler = struct {
    enclosing: ?*Compiler,
    function: *Function,
    function_type: FunctionType,

    locals: std.ArrayList(Local),
    upvalues: [256]Upvalue,
    scopeDepth: u8,

    fn beginScope(this: *Compiler) void {
        this.scopeDepth += 1;
    }

    fn endScope(this: *Compiler) !void {
        this.scopeDepth -= 1;

        while (this.locals.items.len > 0 and
            this.locals.getLast().depth > this.scopeDepth)
        {
            if (this.locals.getLast().isCaptured) {
                try this.currentChunk().data.append(@intFromEnum(Op.CLOSE_UPVALUE));
            } else {
                try this.currentChunk().data.append(@intFromEnum(Op.POP));
            }
            _ = this.locals.pop();
        }
    }

    fn markInitialized(current: *Compiler) void {
        if (current.scopeDepth == 0) return;
        var last = current.locals.items.len - 1;
        current.locals.items[last].depth = current.scopeDepth;
    }

    fn currentChunk(current: *Compiler) *Chunk {
        return &current.function.chunk;
    }

    fn endCompiler(current: *Compiler) !*Function {
        try current.currentChunk().data.append(@intFromEnum(Op.NIL));
        try current.currentChunk().data.append(@intFromEnum(Op.RETURN));
        const fun = current.function;
        current.* = current.enclosing.?.*;
        return fun;
    }
};

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var gen = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa = gen.allocator();

    var file = try std.fs.cwd().openFile("foo.txt", .{});
    defer file.close();

    const file_buffer = try file.readToEndAlloc(gpa, 1024);

    var scanner = Scanner{ .start = 0, .current = 0, .line = 1, .buf = file_buffer };
    var compilingChunk = Chunk{ .data = std.ArrayList(u8).init(gpa), .constants = std.ArrayList(Value).init(gpa), .line = 1 };
    var fun = Function{ .arity = 0, .upvalueCount = 0, .chunk = compilingChunk, .name = "<script>" };
    var compiler = Compiler{ .upvalues = undefined, .enclosing = null, .function = &fun, .function_type = FunctionType.script, .locals = std.ArrayList(Local).init(gpa), .scopeDepth = 0 };
    var parser: Parser = undefined;

    advanceP(&parser, &scanner);
    while (true) {
        if (parser.current.typ == TokenType.EOF) {
            break;
        }
        try stdout.print("declaration? {}\n", .{parser.current.typ});
        try bw.flush();
        try declaration(gpa, &parser, &scanner, &compiler);
    }

    {
        var chunks = std.MultiArrayList(Chunk){};
        var chk = compiler.currentChunk();
        try chunks.append(gpa, chk.*);
        //try stdout.print("chunks: {d}\n", .{chunks.len});
        try disassembleChunk(gpa, chk.data.items, chk.constants.items);
    }
    var vm = VM{
        .frames = std.ArrayList(CallFrame).init(gpa),
        .stack = undefined,
        .stackTop = 0,
        .openUpvalues = null,
    };
    const clj = try newClosure(gpa, &fun);
    vm.stackTop += 1;
    vm.stack[vm.stackTop] = Value{ .closure = clj };

    //try vm.frames.append(CallFrame{ .ip = 0, .slots = &vm.stack, .function = &fun });
    _ = try call(clj, 0, &vm);

    try runChunk(gpa, &vm);
    vm.stackTop = 0;

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
    frames: std.ArrayList(CallFrame),
    stackTop: usize,
    stack: [256]Value,
    openUpvalues: ?*ObjUpvalue,
};

fn get_function(val: Value) !*Function {
    switch (val) {
        .function => |fun| return fun,
        else => @panic("not a function"),
    }
}

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
        .function => |_| return false,
        .closure => |_| return false,
        .upvalue => |_| return false,
    }
}

fn isFalsey(value: Value) bool {
    switch (value) {
        .nil => return true,
        .boolean => |boo| return !boo,
        .function => return true, // Not sure, TODO
        else => return false,
    }
}

fn showVal(gpa: std.mem.Allocator, val: Value) ![]const u8 {
    switch (val) {
        .nil => return "nil",
        .boolean => |b| if (b) {
            return "true";
        } else {
            return "false";
        },
        .number => |n| {
            return try std.fmt.allocPrint(gpa, "{d}", .{n});
        },
        .string => |str| return str,
        .function => |fun| return try std.fmt.allocPrint(gpa, "<fn {s}>", .{fun.name}),
        .closure => |clj| return try std.fmt.allocPrint(gpa, "<closure {s}>", .{clj.function.name}),
        .upvalue => |_| return "upvalue",
    }
}

fn runChunk(gpa: std.mem.Allocator, vm: *VM) !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var globals = std.StringHashMap(Value).init(gpa);

    var lst: Op = undefined;
    var frame: *CallFrame = &vm.frames.items[vm.frames.items.len - 1];
    while (true) {
        const chunk: *Chunk = &frame.closure.function.chunk;
        if (frame.ip >= chunk.data.items.len) {
            //try stdout.print("breaking, ip out of range {d} {d}\n", .{ frame.ip, chunk.data.items.len });
            break;
        }
        const op = @enumFromInt(Op, chunk.data.items[frame.ip]);
        try stdout.print("executing ip={d}, op={s}\n", .{ frame.ip, @tagName(op) });
        //try stdout.print("stack elem 0: ", .{});
        //try printValue(stdout, vm.stack[0]);
        //try stdout.print("stack elem 1: ", .{});
        //try printValue(stdout, vm.stack[1]);
        //try stdout.print("stack elem 2: ", .{});
        //try printValue(stdout, vm.stack[2]);
        //try stdout.print("stack elem 3: ", .{});
        //try printValue(stdout, vm.stack[3]);
        //try stdout.print("stack elem 4: ", .{});
        //try printValue(stdout, vm.stack[4]);
        //try stdout.print("stack elem 5: ", .{});
        //try printValue(stdout, vm.stack[5]);
        frame.ip += 1;
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
            .GET_UPVALUE => {
                const slot = chunk.data.items[frame.ip];
                frame.ip += 1;
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = frame.closure.upvalues.items[slot].?.location.*;
                try stdout.print("got upvalue from slot {d}, saved at {d}: {s}\n", .{ slot, vm.stackTop, try showVal(gpa, vm.stack[vm.stackTop]) });
            },
            .SET_UPVALUE => {
                const slot = chunk.data.items[frame.ip];
                frame.ip += 1;
                frame.closure.upvalues.items[slot].?.location.* = vm.stack[vm.stackTop];
                try stdout.print("set upvalue, saved from stack position {d} at slot={d}: {s}\n", .{ vm.stackTop, slot, try showVal(gpa, vm.stack[vm.stackTop]) });
                //vm.stackTop -= 1;
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
                try stdout.print("less: reading first argument from stack index {d}\n", .{vm.stackTop});
                try stdout.print("less: {d} < {d}\n", .{ a, b });
            },
            .NOT => {
                const b = vm.stack[vm.stackTop];
                vm.stack[vm.stackTop] = Value{ .boolean = isFalsey(b) };
            },
            .CONSTANT => {
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = chunk.constants.items[@as(usize, chunk.data.items[frame.ip])];
                frame.ip += 1;
                try stdout.print("constant: {s}, stored at stackTop={d}\n", .{ try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });
            },
            .MULTIPLY => {
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a * b };
                try stdout.print("mult: a={d} b={d} res={s}, new reduced stackTop={d} \n", .{ a, b, try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });
            },
            .DIVIDE => {
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a / b };
                try stdout.print("divide: a={d} b={d} res={s}, new reduced stackTop={d} \n", .{ a, b, try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });
            },
            .NEGATE => {
                const old = try get_number(vm.stack[vm.stackTop]);
                const res = -old;
                try stdout.print("negate: old={d} res={d}, stackTop={d} \n", .{ old, res, vm.stackTop });
                vm.stack[vm.stackTop] = Value{ .number = res };
            },
            .CLOSURE => {
                const constidx = @as(usize, chunk.data.items[frame.ip]);
                const fun: *Function = try get_function(chunk.constants.items[constidx]);
                try stdout.print("executing closure: got function from ip {d} and constant index {d}: {s}\n", .{ frame.ip, constidx, fun.name });
                //_ = try disassClosure(chunk.data.items[frame.ip..], chunk.constants.items);
                frame.ip += 1;
                const clj: *Closure = try newClosure(gpa, fun);

                vm.stackTop += 1;
                vm.stack[vm.stackTop] = Value{ .closure = clj };

                try stdout.print("run time upvalues count: {d}\n", .{clj.upvalues.items.len});
                for (0..clj.upvalues.items.len) |i| {
                    const isLocal = chunk.data.items[frame.ip];
                    frame.ip += 1;
                    const index = chunk.data.items[frame.ip];
                    frame.ip += 1;
                    try stdout.print("isLocal: {d}, index at chunk->code idx {d}: {d}\n", .{ isLocal, frame.ip - 1, index });
                    if (isLocal == 1) {
                        clj.upvalues.items[i] = try captureUpvalue(gpa, &vm.stack[frame.slotsOffset + index + 1], vm);
                    } else {
                        try stdout.print("reading from frame upvalues, which has length {d}\n", .{frame.closure.upvalues.items.len});
                        clj.upvalues.items[i] = frame.closure.upvalues.items[index];
                    }
                }
            },
            .CLOSE_UPVALUE => {
                closeUpvalues(&vm.stack[vm.stackTop], vm);
                vm.stackTop -= 1;
            },
            .RETURN => {
                try stdout.print("return: {s} read from stackTop={d}\n", .{ try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });

                // pop
                const result = vm.stack[vm.stackTop];
                vm.stackTop -= 1;

                closeUpvalues(&vm.stack[frame.slotsOffset + 1], vm);

                if (vm.frames.items.len == 1) {
                    vm.stackTop -= 1; // pop
                    @panic("INTERPRET OK (should quit)");
                }
                _ = vm.frames.pop();

                vm.stackTop = frame.slotsOffset;

                // push
                vm.stackTop += 1;
                vm.stack[vm.stackTop] = result;

                try stdout.print("return: wrote result {s} to stack position {d}\n", .{ try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });
                try stdout.print("return: switching to frame {d}\n", .{vm.frames.items.len - 1});
                frame = &vm.frames.items[vm.frames.items.len - 1];
            },
            .SUBTRACT => {
                try stdout.print("sub: original stackTop={d}\n", .{vm.stackTop});
                const b = try get_number(vm.stack[vm.stackTop - 0]);
                const a = try get_number(vm.stack[vm.stackTop - 1]);
                vm.stackTop -= 1;
                vm.stack[vm.stackTop] = Value{ .number = a - b };
                try stdout.print("sub: a={d} b={d} res={d}, new reduced stackTop={d}\n", .{ a, b, try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });
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
                        try stdout.print("add: a={d} b={d} res={s}, stored result at new reduced stackTop={d}\n", .{ an, bn, try showVal(gpa, vm.stack[vm.stackTop]), vm.stackTop });
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
                    .function => |_| {
                        @panic("invalid function for + operator");
                    },
                    .closure => |_| {
                        @panic("invalid closure for + operator");
                    },
                    .upvalue => |_| {
                        @panic("invalid upvalue for + operator");
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
                const name = try get_string(chunk.constants.items[@as(usize, chunk.data.items[frame.ip])]);
                frame.ip += 1;
                //try stdout.print("defining global {s}\n", .{name});
                //try printValue(stdout, vm.stack[vm.stackTop]);
                try globals.putNoClobber(name, vm.stack[vm.stackTop]);
                vm.stackTop -= 1;
                try stdout.print("defining global {s}, decresed stack from {d} (read from here) to {d}\n", .{ name, vm.stackTop + 1, vm.stackTop });
            },
            .GET_GLOBAL => {
                const constidx = @as(usize, chunk.data.items[frame.ip]);
                frame.ip += 1;
                const name = try get_string(chunk.constants.items[constidx]);
                try stdout.print("getting global using constant idx {d}. It is '{s}'\n", .{ constidx, name });
                var val = globals.get(name);
                if (val) |v| {
                    vm.stackTop += 1;
                    try stdout.print("setting stack index {d}\n", .{vm.stackTop});
                    vm.stack[vm.stackTop] = v;
                } else {
                    @panic("global not found");
                }
            },
            .SET_GLOBAL => {
                const constidx = @as(usize, chunk.data.items[frame.ip]);
                frame.ip += 1;
                const name = try get_string(chunk.constants.items[constidx]);
                try stdout.print("writing global '{s}'\n", .{name});
                if (!globals.contains(name)) {
                    @panic("global not defined yet, can't assign to it");
                    // chapter 21:
                    //  If the variable hasn’t been defined yet, it’s a
                    //  runtime error to try to assign to it. Lox doesn’t do
                    //  implicit variable declaration.
                }
                try globals.put(name, vm.stack[vm.stackTop]);
            },
            .GET_LOCAL => {
                const slot = @as(usize, chunk.data.items[frame.ip]);
                try stdout.print("getting local from slot {d}\n", .{slot + 1});
                frame.ip += 1;
                vm.stackTop += 1;
                try stdout.print("storing local at {d}: {s}\n", .{ vm.stackTop, try showVal(gpa, vm.stack[slot + 1]) });
                // slot is off by one because we are incrementing stackTop
                // before use in the CONSTANT branch as if it didn't point
                // past the last elem. This plus one shouldn't be there
                vm.stack[vm.stackTop] = vm.stack[frame.slotsOffset + slot + 1];
            },
            .SET_LOCAL => {
                const slot = @as(usize, chunk.data.items[frame.ip]);
                frame.ip += 1;
                vm.stack[frame.slotsOffset + slot + 1] = vm.stack[vm.stackTop];
                try stdout.print("setting local at slot {d} to value at stack index {d}: {s}\n", .{ slot + 1, vm.stackTop, try showVal(gpa, vm.stack[vm.stackTop]) });
            },
            .JUMP => {
                frame.ip += 2;
                const hi = chunk.data.items[frame.ip - 2];
                const lo = chunk.data.items[frame.ip - 1];
                const offset = @intCast(u16, hi) << 8 | @intCast(u16, lo);
                frame.ip += offset;
            },
            .JUMP_IF_FALSE => {
                frame.ip += 2;
                const hi = chunk.data.items[frame.ip - 2];
                const lo = chunk.data.items[frame.ip - 1];
                const offset = @intCast(u16, hi) << 8 | @intCast(u16, lo);
                if (isFalsey(vm.stack[vm.stackTop])) frame.ip += offset;
            },
            .LOOP => {
                frame.ip += 2;
                const hi = chunk.data.items[frame.ip - 2];
                const lo = chunk.data.items[frame.ip - 1];
                const offset = @intCast(u16, hi) << 8 | @intCast(u16, lo);
                frame.ip -= offset;
            },
            .CALL => {
                const argCount = chunk.data.items[frame.ip];
                frame.ip += 1;
                const functionToCall = vm.stack[vm.stackTop - argCount];
                if (!try callValue(functionToCall, argCount, vm)) {
                    @panic("CallValue returned false, are you calling a non-function?");
                }
                const idx = vm.frames.items.len - 1;
                try stdout.print("returning to frame {d}\n", .{idx});
                frame = &vm.frames.items[idx];
            },
        }
    }
    try bw.flush();
}

fn call(clj: *Closure, argCount: u8, vm: *VM) !bool {
    //_ = try disassClosure(clj.function.chunk.data.items, clj.function.chunk.constants.items);
    if (argCount != clj.function.arity) {
        @panic("function arity mismatch");
    }
    var frame = CallFrame{ .closure = clj, .ip = 0, .slotsOffset = vm.stackTop - argCount - 1 };
    try vm.frames.append(frame);
    return true;
}

fn callValue(callee: Value, argCount: u8, vm: *VM) !bool {
    switch (callee) {
        .closure => |clj| {
            return try call(clj, argCount, vm);
        },
        else => return false,
    }
}

fn captureUpvalue(gpa: std.mem.Allocator, local: *Value, vm: *VM) !*ObjUpvalue {
    var prevUpvalue: ?*ObjUpvalue = null;
    var upvalue: ?*ObjUpvalue = vm.openUpvalues;
    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prevUpvalue = upvalue.?;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and upvalue.?.location == local) {
        return upvalue.?;
    }

    const createdUpvalue: *ObjUpvalue = try newUpvalue(gpa, local);
    createdUpvalue.next = upvalue;

    if (prevUpvalue == null) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue.?.next = createdUpvalue;
    }

    return createdUpvalue;
}

fn closeUpvalues(last: *Value, vm: *VM) void {
    while (vm.openUpvalues != null and @intFromPtr(vm.openUpvalues.?.location) >= @intFromPtr(last)) {
        var upvalue: *ObjUpvalue = vm.openUpvalues.?;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        vm.openUpvalues = upvalue.next;
    }
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
    CLOSURE,
    CLOSE_UPVALUE,
    RETURN,
    PRINT,
    JUMP,
    JUMP_IF_FALSE,
    LOOP,
    CALL,
    POP,
    DEFINE_GLOBAL,
    GET_GLOBAL,
    SET_GLOBAL,
    GET_UPVALUE,
    SET_UPVALUE,
    GET_LOCAL,
    SET_LOCAL,
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
