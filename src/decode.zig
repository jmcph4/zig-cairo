const std = @import("std");

pub const Flags = struct {
    dst_reg: u1,
    op0_reg: u1,
    op1_src: u3,
    res_logic: u2,
    pc_update: u3,
    ap_update: u2,
    opcode: Opcode,
};

pub const Instruction = struct {
    off_dst: u16,
    off_op0: u16,
    off_op1: u16,
    flags: Flags,
    imm: ?u64,
};

pub const Opcode = enum(u3) {
    NOP = 0x00,
    CALL = 0x01,
    RET = 0x02,
    ASSERT_EQ = 0x04,

    /// Does this opcode use an immediate value?
    fn immediate(self: Opcode) bool {
        _ = self; // autofix

        return false; // TODO(jmcph4): fix
    }
};

const FlagMask = enum(u16) {
    DST_REG = 0x0001,
    OP0_REG = 0x0002,
    OP1_SRC = 0x001C,
    RES_LOGIC = 0x0060,
    PC_UPDATE = 0x0380,
    AP_UPDATE = 0x0C00,
    OPCODE = 0x7000,
};

const FlagOffset = enum(u16) {
    DST_REG = 0,
    OP0_REG = 1,
    OP1_SRC = 2,
    RES_LOGIC = 5,
    PC_UPDATE = 7,
    AP_UPDATE = 10,
    OPCODE = 12,
};

pub fn decode_flags(bytes: u16) Flags {
    return Flags{
        .dst_reg = @truncate((bytes & @intFromEnum(FlagMask.DST_REG)) >> @intFromEnum(FlagOffset.DST_REG)),
        .op0_reg = @truncate((bytes & @intFromEnum(FlagMask.OP0_REG)) >> @intFromEnum(FlagOffset.OP0_REG)),
        .op1_src = @truncate((bytes & @intFromEnum(FlagMask.OP1_SRC)) >> @intFromEnum(FlagOffset.OP1_SRC)),
        .res_logic = @truncate((bytes & @intFromEnum(FlagMask.RES_LOGIC)) >> @intFromEnum(FlagOffset.RES_LOGIC)),
        .pc_update = @truncate((bytes & @intFromEnum(FlagMask.PC_UPDATE)) >> @intFromEnum(FlagOffset.PC_UPDATE)),
        .ap_update = @truncate((bytes & @intFromEnum(FlagMask.AP_UPDATE)) >> @intFromEnum(FlagOffset.AP_UPDATE)),
        .opcode = @enumFromInt(@as(u16, (bytes & @intFromEnum(FlagMask.OPCODE)) >> @intFromEnum(FlagOffset.OPCODE))),
    };
}

pub fn decode_values(word: u64) Instruction {
    const OFF_DST_OFF: u64 = 0;
    const OFF_OP0_OFF: u64 = 16;
    const OFF_OP1_OFF: u64 = 32;
    const FLAGS_OFF: u64 = 48;

    return Instruction{
        .off_dst = @truncate((word >> OFF_DST_OFF) & 0xFFFF),
        .off_op0 = @truncate((word >> OFF_OP0_OFF) & 0xFFFF),
        .off_op1 = @truncate((word >> OFF_OP1_OFF) & 0xFFFF),
        .flags = decode_flags(@truncate((word >> FLAGS_OFF) & 0xFFFF)),
        .imm = null,
    };
}

pub fn decode(code: []const u64, allocator: std.mem.Allocator) anyerror!std.ArrayList(Instruction) {
    var instructions: std.ArrayList(Instruction) = std.ArrayList(Instruction).init(allocator);

    var i: usize = 0;

    while (i < code.len) {
        const curr_word: u64 = code[i];
        var curr_inst: Instruction = decode_values(curr_word);

        if (curr_inst.flags.opcode.immediate()) {
            curr_inst.imm = code[i + 1];
            i += 1; // move our cursor so we skip this word as we've already captured it as an immediate value
        }

        try instructions.append(curr_inst);

        i += 1;
    }

    return instructions;
}

test "decode_flags_call_add_jmp_add_imm_fp_fp" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x14A7800080008000};
    const instructions: std.ArrayList(Instruction) = try decode(&code, gpa.allocator());
    defer instructions.deinit();
    const instruction: Instruction = instructions.items[0];

    // offsets
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_dst);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op0);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op1);

    // flags
    try std.testing.expectEqual(@as(u1, 1), instruction.flags.dst_reg);
    try std.testing.expectEqual(@as(u1, 1), instruction.flags.op0_reg);
    try std.testing.expectEqual(@as(u3, 1), instruction.flags.op1_src);
    try std.testing.expectEqual(@as(u2, 1), instruction.flags.res_logic);
    try std.testing.expectEqual(@as(u3, 1), instruction.flags.pc_update);
    try std.testing.expectEqual(@as(u2, 1), instruction.flags.ap_update);
    try std.testing.expectEqual(Opcode.CALL, instruction.flags.opcode);

    // immediate
    try std.testing.expect(instruction.imm == null); // should be expectEqual but Zig is broken atm (see: https://github.com/ziglang/zig/pull/17431)
}

test "decode_flags_ret_add1_jmp_rel_mul_fp_ap_ap" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x2948800080008000};
    const instructions: std.ArrayList(Instruction) = try decode(&code, gpa.allocator());
    defer instructions.deinit();
    const instruction: Instruction = instructions.items[0];

    // offsets
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_dst);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op0);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op1);

    // flags
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.dst_reg);
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.op0_reg);
    try std.testing.expectEqual(@as(u3, 2), instruction.flags.op1_src);
    try std.testing.expectEqual(@as(u2, 2), instruction.flags.res_logic);
    try std.testing.expectEqual(@as(u3, 2), instruction.flags.pc_update);
    try std.testing.expectEqual(@as(u2, 2), instruction.flags.ap_update);
    try std.testing.expectEqual(Opcode.RET, instruction.flags.opcode);

    // immediate
    try std.testing.expect(instruction.imm == null); // should be expectEqual but Zig is broken atm (see: https://github.com/ziglang/zig/pull/17431)
}

test "decode_flags_assrt_add_jnz_mul_ap_ap_ap" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x4A50800080008000};
    const instructions: std.ArrayList(Instruction) = try decode(&code, gpa.allocator());
    defer instructions.deinit();
    const instruction: Instruction = instructions.items[0];

    // offsets
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_dst);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op0);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op1);

    // flags
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.dst_reg);
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.op0_reg);
    try std.testing.expectEqual(@as(u3, 4), instruction.flags.op1_src);
    try std.testing.expectEqual(@as(u2, 2), instruction.flags.res_logic);
    try std.testing.expectEqual(@as(u3, 4), instruction.flags.pc_update);
    try std.testing.expectEqual(@as(u2, 2), instruction.flags.ap_update);
    try std.testing.expectEqual(Opcode.ASSERT_EQ, instruction.flags.opcode);

    // immediate
    try std.testing.expect(instruction.imm == null); // should be expectEqual but Zig is broken atm (see: https://github.com/ziglang/zig/pull/17431)
}

test "decode_flags_assrt_add2_jnz_uncon_op0_ap_ap" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x4200800080008000};
    const instructions: std.ArrayList(Instruction) = try decode(&code, gpa.allocator());
    defer instructions.deinit();
    const instruction: Instruction = instructions.items[0];

    // offsets
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_dst);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op0);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op1);

    // flags
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.dst_reg);
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.op0_reg);
    try std.testing.expectEqual(@as(u3, 0), instruction.flags.op1_src);
    try std.testing.expectEqual(@as(u2, 0), instruction.flags.res_logic);
    try std.testing.expectEqual(@as(u3, 4), instruction.flags.pc_update);
    try std.testing.expectEqual(@as(u2, 0), instruction.flags.ap_update);
    try std.testing.expectEqual(Opcode.ASSERT_EQ, instruction.flags.opcode);

    // immediate
    try std.testing.expect(instruction.imm == null); // should be expectEqual but Zig is broken atm (see: https://github.com/ziglang/zig/pull/17431)
}

test "decode_flags_nop_regu_regu_op1_op0_ap_ap" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x0000800080008000};
    const instructions: std.ArrayList(Instruction) = try decode(&code, gpa.allocator());
    defer instructions.deinit();
    const instruction: Instruction = instructions.items[0];

    // offsets
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_dst);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op0);
    try std.testing.expectEqual(@as(u16, 0x8000), instruction.off_op1);

    // flags
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.dst_reg);
    try std.testing.expectEqual(@as(u1, 0), instruction.flags.op0_reg);
    try std.testing.expectEqual(@as(u3, 0), instruction.flags.op1_src);
    try std.testing.expectEqual(@as(u2, 0), instruction.flags.res_logic);
    try std.testing.expectEqual(@as(u3, 0), instruction.flags.pc_update);
    try std.testing.expectEqual(@as(u2, 0), instruction.flags.ap_update);
    try std.testing.expectEqual(Opcode.NOP, instruction.flags.opcode);

    // immediate
    try std.testing.expect(instruction.imm == null); // should be expectEqual but Zig is broken atm (see: https://github.com/ziglang/zig/pull/17431)
}
