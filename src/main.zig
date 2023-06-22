const std = @import("std");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var buffer: [1000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const gpa = fba.allocator();
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
