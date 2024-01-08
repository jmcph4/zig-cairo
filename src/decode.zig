const std = @import("std");

pub const types = @import("types.zig");

const Instruction = types.Instruction;
const Flags = types.Flags;
const Register = types.Register;
const Op1Address = types.Op1Address;
const Res = types.Res;
const PcUpdate = types.PcUpdate;
const ApUpdate = types.ApUpdate;
const Opcode = types.Opcode;

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

pub fn decode_flags(bytes: u16) DecodeError!Flags {
    const HIGH_BIT: u16 = 1 << 15;

    if (bytes & HIGH_BIT != 0) {
        return DecodeError.NON_ZERO_HIGH_BIT;
    }

    const pc_update: ?PcUpdate = PcUpdate.from_u3(@truncate((bytes & @intFromEnum(FlagMask.PC_UPDATE)) >> @intFromEnum(FlagOffset.PC_UPDATE)));

    if (pc_update == null) {
        return DecodeError.INVALID_PC_UPDATE;
    }

    const opcode: ?Opcode = Opcode.from_u3(@truncate(@as(u16, (bytes & @intFromEnum(FlagMask.OPCODE)) >> @intFromEnum(FlagOffset.OPCODE))));

    if (opcode == null) {
        return DecodeError.INVALID_OPCODE;
    }

    const op1_src: ?Op1Address = Op1Address.from_u3(@truncate((bytes & @intFromEnum(FlagMask.OP1_SRC)) >> @intFromEnum(FlagOffset.OP1_SRC)));

    if (op1_src == null) {
        return DecodeError.INVALID_OP1_SRC;
    }

    const res_logic: ?Res = Res.init(@truncate((bytes & @intFromEnum(FlagMask.RES_LOGIC)) >> @intFromEnum(FlagOffset.RES_LOGIC)), pc_update.?);

    if (res_logic == null) {
        return DecodeError.INVALID_RES_LOGIC;
    }

    const ap_update: ?ApUpdate = ApUpdate.init(@truncate((bytes & @intFromEnum(FlagMask.AP_UPDATE)) >> @intFromEnum(FlagOffset.AP_UPDATE)), opcode.?);

    if (ap_update == null) {
        return DecodeError.INVALID_AP_UPDATE;
    }

    return Flags{
        .dst_reg = Register.from_u1(@truncate((bytes & @intFromEnum(FlagMask.DST_REG)) >> @intFromEnum(FlagOffset.DST_REG))),
        .op0_reg = Register.from_u1(@truncate((bytes & @intFromEnum(FlagMask.OP0_REG)) >> @intFromEnum(FlagOffset.OP0_REG))),
        .op1_src = op1_src.?,
        .res_logic = res_logic.?,
        .pc_update = pc_update.?,
        .ap_update = ap_update.?,
        .opcode = opcode.?,
    };
}

pub fn decode_values(word: u64) !Instruction {
    const OFF_DST_OFF: u64 = 0;
    const OFF_OP0_OFF: u64 = 16;
    const OFF_OP1_OFF: u64 = 32;
    const FLAGS_OFF: u64 = 48;

    return Instruction{
        .off_dst = @truncate((word >> OFF_DST_OFF) & 0xFFFF),
        .off_op0 = @truncate((word >> OFF_OP0_OFF) & 0xFFFF),
        .off_op1 = @truncate((word >> OFF_OP1_OFF) & 0xFFFF),
        .flags = try decode_flags(@truncate((word >> FLAGS_OFF) & 0xFFFF)),
        .imm = null,
    };
}

pub const DecodeError = error{
    INVALID_OPCODE,
    INVALID_PC_UPDATE,
    INVALID_AP_UPDATE,
    INVALID_OP1_SRC,
    INVALID_RES_LOGIC,
    NON_ZERO_HIGH_BIT,
};

pub fn decode(code: []const u64, allocator: std.mem.Allocator) !std.ArrayList(Instruction) {
    var instructions: std.ArrayList(Instruction) = std.ArrayList(Instruction).init(allocator);

    var i: usize = 0;

    while (i < code.len) {
        const curr_word: u64 = code[i];
        var curr_inst: Instruction = try decode_values(curr_word);

        if (curr_inst.flags.opcode.immediate()) {
            curr_inst.imm = code[i + 1];
            i += 1; // move our cursor so we skip this word as we've already captured it as an immediate value
        }

        try instructions.append(curr_inst);

        i += 1;
    }

    return instructions;
}

test "non_zero_high_bit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x94A7800080008000};

    try std.testing.expectError(DecodeError.NON_ZERO_HIGH_BIT, decode(&code, gpa.allocator()));
}

test "invalid_op1_reg" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x294F800080008000};

    try std.testing.expectError(DecodeError.INVALID_OP1_SRC, decode(&code, gpa.allocator()));
}

test "invalid_pc_update" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x29A8800080008000};

    try std.testing.expectError(DecodeError.INVALID_PC_UPDATE, decode(&code, gpa.allocator()));
}

test "invalid_res_logic" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x2968800080008000};

    try std.testing.expectError(DecodeError.INVALID_RES_LOGIC, decode(&code, gpa.allocator()));
}

test "invalid_opcode" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x3948800080008000};

    try std.testing.expectError(DecodeError.INVALID_OPCODE, decode(&code, gpa.allocator()));
}

test "invalid_ap_update" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const code: [1]u64 = [1]u64{0x2D48800080008000};

    try std.testing.expectError(DecodeError.INVALID_AP_UPDATE, decode(&code, gpa.allocator()));
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
    try std.testing.expectEqual(Register.FP, instruction.flags.dst_reg);
    try std.testing.expectEqual(Register.FP, instruction.flags.op0_reg);
    try std.testing.expectEqual(Op1Address.IMMEDIATE, instruction.flags.op1_src);
    try std.testing.expectEqual(Res.ADD, instruction.flags.res_logic);
    try std.testing.expectEqual(PcUpdate.JUMP_IMMEDIATE, instruction.flags.pc_update);
    try std.testing.expectEqual(ApUpdate.ADD, instruction.flags.ap_update);
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
    try std.testing.expectEqual(Register.AP, instruction.flags.dst_reg);
    try std.testing.expectEqual(Register.AP, instruction.flags.op0_reg);
    try std.testing.expectEqual(Op1Address.FP, instruction.flags.op1_src);
    try std.testing.expectEqual(Res.MUL, instruction.flags.res_logic);
    try std.testing.expectEqual(PcUpdate.JUMP_RELATIVE, instruction.flags.pc_update);
    try std.testing.expectEqual(ApUpdate.ADD1, instruction.flags.ap_update);
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
    try std.testing.expectEqual(Register.AP, instruction.flags.dst_reg);
    try std.testing.expectEqual(Register.AP, instruction.flags.op0_reg);
    try std.testing.expectEqual(Op1Address.AP, instruction.flags.op1_src);
    try std.testing.expectEqual(Res.MUL, instruction.flags.res_logic);
    try std.testing.expectEqual(PcUpdate.JNZ, instruction.flags.pc_update);
    try std.testing.expectEqual(ApUpdate.ADD1, instruction.flags.ap_update);
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
    try std.testing.expectEqual(Register.AP, instruction.flags.dst_reg);
    try std.testing.expectEqual(Register.AP, instruction.flags.op0_reg);
    try std.testing.expectEqual(Op1Address.OP0, instruction.flags.op1_src);
    try std.testing.expectEqual(Res.UNCONSTRAINED, instruction.flags.res_logic);
    try std.testing.expectEqual(PcUpdate.JNZ, instruction.flags.pc_update);
    try std.testing.expectEqual(ApUpdate.REGULAR, instruction.flags.ap_update);
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
    try std.testing.expectEqual(Register.AP, instruction.flags.dst_reg);
    try std.testing.expectEqual(Register.AP, instruction.flags.op0_reg);
    try std.testing.expectEqual(Op1Address.OP0, instruction.flags.op1_src);
    try std.testing.expectEqual(Res.OP1, instruction.flags.res_logic);
    try std.testing.expectEqual(PcUpdate.REGULAR, instruction.flags.pc_update);
    try std.testing.expectEqual(ApUpdate.REGULAR, instruction.flags.ap_update);
    try std.testing.expectEqual(Opcode.NOP, instruction.flags.opcode);

    // immediate
    try std.testing.expect(instruction.imm == null); // should be expectEqual but Zig is broken atm (see: https://github.com/ziglang/zig/pull/17431)
}
