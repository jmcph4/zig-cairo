pub const Flags = struct {
    dst_reg: Register,
    op0_reg: Register,
    op1_src: Op1Address,
    res_logic: Res,
    pc_update: PcUpdate,
    ap_update: ApUpdate,
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
    pub fn immediate(self: Opcode) bool {
        _ = self; // autofix

        return false; // TODO(jmcph4): fix
    }

    pub fn from_u3(x: u3) ?Opcode {
        if (x > 0x04 or x == 0x03) {
            return null;
        } else {
            return @enumFromInt(x);
        }
    }
};

pub const Register = enum(u1) {
    AP = 0x00,
    FP = 0x01,

    pub fn from_u1(x: u1) Register {
        return @enumFromInt(x);
    }
};

pub const PcUpdate = enum(u3) {
    REGULAR = 0x00,
    JUMP_IMMEDIATE = 0x01,
    JUMP_RELATIVE = 0x02,
    JNZ = 0x04,

    pub fn from_u3(x: u3) ?PcUpdate {
        if (x > 0x04 or x == 0x03) {
            return null;
        } else {
            return @enumFromInt(x);
        }
    }
};

pub const Res = enum(u2) {
    OP1 = 0x00,
    ADD = 0x01,
    MUL = 0x02,
    UNCONSTRAINED,

    pub fn init(x: u2, pc_update: PcUpdate) ?Res {
        if (x == 0x00) {
            if (pc_update == PcUpdate.JNZ) {
                return Res.UNCONSTRAINED;
            } else {
                return Res.OP1;
            }
        } else if (x == 0x01) {
            return Res.ADD;
        } else if (x == 0x02) {
            return Res.MUL;
        } else {
            return null;
        }
    }
};

pub const Op1Address = enum(u3) {
    OP0 = 0x00,
    IMMEDIATE = 0x01,
    FP = 0x02,
    AP = 0x04,

    pub fn from_u3(x: u3) ?Op1Address {
        if (x > 0x04 or x == 0x03) {
            return null;
        } else {
            return @enumFromInt(x);
        }
    }
};

pub const ApUpdate = enum(u2) {
    REGULAR,
    ADD,
    ADD1,
    ADD2,

    pub fn init(x: u2, opcode: Opcode) ?ApUpdate {
        if (x == 0x00) {
            if (opcode == Opcode.CALL) {
                return ApUpdate.ADD2;
            } else {
                return ApUpdate.REGULAR;
            }
        } else if (x == 0x01) {
            return ApUpdate.ADD;
        } else if (x == 0x02) {
            return ApUpdate.ADD1;
        } else {
            return null;
        }
    }
};

pub const FpUpdate = enum(u3) {
    REGULAR,
    AP_PLUS_2,
    DST,

    pub fn from_opcode(opcode: Opcode) FpUpdate {
        switch (opcode) {
            Opcode.CALL => FpUpdate.AP_PLUS_2,
            Opcode.RET => FpUpdate.DST,
            _ => FpUpdate.REGULAR,
        }
    }
};
