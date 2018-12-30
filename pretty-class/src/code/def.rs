use class::{ByteParser, ParseError};

pub const NOP: u8 = 0x00;
pub const ACONST_NULL: u8 = 0x01;
pub const ICONST_M1: u8 = 0x02;
pub const ICONST_0: u8 = 0x03;
pub const ICONST_1: u8 = 0x04;
pub const ICONST_2: u8 = 0x05;
pub const ICONST_3: u8 = 0x06;
pub const ICONST_4: u8 = 0x07;
pub const ICONST_5: u8 = 0x08;
pub const LCONST_0: u8 = 0x09;
pub const LCONST_1: u8 = 0x0a;
pub const FCONST_0: u8 = 0x0b;
pub const FCONST_1: u8 = 0x0c;
pub const FCONST_2: u8 = 0x0d;
pub const DCONST_0: u8 = 0x0e;
pub const DCONST_1: u8 = 0x0f;
pub const BIPUSH: u8 = 0x10;
pub const SIPUSH: u8 = 0x11;
pub const LDC: u8 = 0x12;
pub const LDC_W: u8 = 0x13;
pub const LDC2_W: u8 = 0x14;
pub const ILOAD: u8 = 0x15;
pub const LLOAD: u8 = 0x16;
pub const FLOAD: u8 = 0x17;
pub const DLOAD: u8 = 0x18;
pub const ALOAD: u8 = 0x19;
pub const ILOAD_0: u8 = 0x1a;
pub const ILOAD_1: u8 = 0x1b;
pub const ILOAD_2: u8 = 0x1c;
pub const ILOAD_3: u8 = 0x1d;
pub const LLOAD_0: u8 = 0x1e;
pub const LLOAD_1: u8 = 0x1f;
pub const LLOAD_2: u8 = 0x20;
pub const LLOAD_3: u8 = 0x21;
pub const FLOAD_0: u8 = 0x22;
pub const FLOAD_1: u8 = 0x23;
pub const FLOAD_2: u8 = 0x24;
pub const FLOAD_3: u8 = 0x25;
pub const DLOAD_0: u8 = 0x26;
pub const DLOAD_1: u8 = 0x27;
pub const DLOAD_2: u8 = 0x28;
pub const DLOAD_3: u8 = 0x29;
pub const ALOAD_0: u8 = 0x2a;
pub const ALOAD_1: u8 = 0x2b;
pub const ALOAD_2: u8 = 0x2c;
pub const ALOAD_3: u8 = 0x2d;
pub const IALOAD: u8 = 0x2e;
pub const LALOAD: u8 = 0x2f;
pub const FALOAD: u8 = 0x30;
pub const DALOAD: u8 = 0x31;
pub const AALOAD: u8 = 0x32;
pub const BALOAD: u8 = 0x33;
pub const CALOAD: u8 = 0x34;
pub const SALOAD: u8 = 0x35;
pub const ISTORE: u8 = 0x36;
pub const LSTORE: u8 = 0x37;
pub const FSTORE: u8 = 0x38;
pub const DSTORE: u8 = 0x39;
pub const ASTORE: u8 = 0x3a;
pub const ISTORE_0: u8 = 0x3b;
pub const ISTORE_1: u8 = 0x3c;
pub const ISTORE_2: u8 = 0x3d;
pub const ISTORE_3: u8 = 0x3e;
pub const LSTORE_0: u8 = 0x3f;
pub const LSTORE_1: u8 = 0x40;
pub const LSTORE_2: u8 = 0x41;
pub const LSTORE_3: u8 = 0x42;
pub const FSTORE_0: u8 = 0x43;
pub const FSTORE_1: u8 = 0x44;
pub const FSTORE_2: u8 = 0x45;
pub const FSTORE_3: u8 = 0x46;
pub const DSTORE_0: u8 = 0x47;
pub const DSTORE_1: u8 = 0x48;
pub const DSTORE_2: u8 = 0x49;
pub const DSTORE_3: u8 = 0x4a;
pub const ASTORE_0: u8 = 0x4b;
pub const ASTORE_1: u8 = 0x4c;
pub const ASTORE_2: u8 = 0x4d;
pub const ASTORE_3: u8 = 0x4e;
pub const IASTORE: u8 = 0x4f;
pub const LASTORE: u8 = 0x50;
pub const FASTORE: u8 = 0x51;
pub const DASTORE: u8 = 0x52;
pub const AASTORE: u8 = 0x53;
pub const BASTORE: u8 = 0x54;
pub const CASTORE: u8 = 0x55;
pub const SASTORE: u8 = 0x56;
pub const POP: u8 = 0x57;
pub const POP2: u8 = 0x58;
pub const DUP: u8 = 0x59;
pub const DUP_X1: u8 = 0x5a;
pub const DUP_X2: u8 = 0x5b;
pub const DUP2: u8 = 0x5c;
pub const DUP2_X1: u8 = 0x5d;
pub const DUP2_X2: u8 = 0x5e;
pub const SWAP: u8 = 0x5f;
pub const IADD: u8 = 0x60;
pub const LADD: u8 = 0x61;
pub const FADD: u8 = 0x62;
pub const DADD: u8 = 0x63;
pub const ISUB: u8 = 0x64;
pub const LSUB: u8 = 0x65;
pub const FSUB: u8 = 0x66;
pub const DSUB: u8 = 0x67;
pub const IMUL: u8 = 0x68;
pub const LMUL: u8 = 0x69;
pub const FMUL: u8 = 0x6a;
pub const DMUL: u8 = 0x6b;
pub const IDIV: u8 = 0x6c;
pub const LDIV: u8 = 0x6d;
pub const FDIV: u8 = 0x6e;
pub const DDIV: u8 = 0x6f;
pub const IREM: u8 = 0x70;
pub const LREM: u8 = 0x71;
pub const FREM: u8 = 0x72;
pub const DREM: u8 = 0x73;
pub const INEG: u8 = 0x74;
pub const LNEG: u8 = 0x75;
pub const FNEG: u8 = 0x76;
pub const DNEG: u8 = 0x77;
pub const ISHL: u8 = 0x78;
pub const LSHL: u8 = 0x79;
pub const ISHR: u8 = 0x7a;
pub const LSHR: u8 = 0x7b;
pub const IUSHR: u8 = 0x7c;
pub const LUSHR: u8 = 0x7d;
pub const IAND: u8 = 0x7e;
pub const LAND: u8 = 0x7f;
pub const IOR: u8 = 0x80;
pub const LOR: u8 = 0x81;
pub const IXOR: u8 = 0x82;
pub const LXOR: u8 = 0x83;
pub const IINC: u8 = 0x84;
pub const I2L: u8 = 0x85;
pub const I2F: u8 = 0x86;
pub const I2D: u8 = 0x87;
pub const L2I: u8 = 0x88;
pub const L2F: u8 = 0x89;
pub const L2D: u8 = 0x8a;
pub const F2I: u8 = 0x8b;
pub const F2L: u8 = 0x8c;
pub const F2D: u8 = 0x8d;
pub const D2I: u8 = 0x8e;
pub const D2L: u8 = 0x8f;
pub const D2F: u8 = 0x90;
pub const I2B: u8 = 0x91;
pub const I2C: u8 = 0x92;
pub const I2S: u8 = 0x93;
pub const LCMP: u8 = 0x94;
pub const FCMPL: u8 = 0x95;
pub const FCMPG: u8 = 0x96;
pub const DCMPL: u8 = 0x97;
pub const DCMPG: u8 = 0x98;
pub const IFEQ: u8 = 0x99;
pub const IFNE: u8 = 0x9a;
pub const IFLT: u8 = 0x9b;
pub const IFGE: u8 = 0x9c;
pub const IFGT: u8 = 0x9d;
pub const IFLE: u8 = 0x9e;
pub const IF_ICMPEQ: u8 = 0x9f;
pub const IF_ICMPNE: u8 = 0xa0;
pub const IF_ICMPLT: u8 = 0xa1;
pub const IF_ICMPGE: u8 = 0xa2;
pub const IF_ICMPGT: u8 = 0xa3;
pub const IF_ICMPLE: u8 = 0xa4;
pub const IF_ACMPEQ: u8 = 0xa5;
pub const IF_ACMPNE: u8 = 0xa6;
pub const GOTO: u8 = 0xa7;
pub const JSR: u8 = 0xa8;
pub const RET: u8 = 0xa9;
pub const TABLESWITCH: u8 = 0xaa;
pub const LOOKUPSWITCH: u8 = 0xab;
pub const IRETURN: u8 = 0xac;
pub const LRETURN: u8 = 0xad;
pub const FRETURN: u8 = 0xae;
pub const DRETURN: u8 = 0xaf;
pub const ARETURN: u8 = 0xb0;
pub const RETURN: u8 = 0xb1;
pub const GETSTATIC: u8 = 0xb2;
pub const PUTSTATIC: u8 = 0xb3;
pub const GETFIELD: u8 = 0xb4;
pub const PUTFIELD: u8 = 0xb5;
pub const INVOKEVIRTUAL: u8 = 0xb6;
pub const INVOKESPECIAL: u8 = 0xb7;
pub const INVOKESTATIC: u8 = 0xb8;
pub const INVOKEINTERFACE: u8 = 0xb9;
pub const INVOKEDYNAMIC: u8 = 0xba;
pub const NEW: u8 = 0xbb;
pub const NEWARRAY: u8 = 0xbc;
pub const ANEWARRAY: u8 = 0xbd;
pub const ARRAYLENGTH: u8 = 0xbe;
pub const ATHROW: u8 = 0xbf;
pub const CHECKCAST: u8 = 0xc0;
pub const INSTANCEOF: u8 = 0xc1;
pub const MONITORENTER: u8 = 0xc2;
pub const MONITOREXIT: u8 = 0xc3;
pub const WIDE: u8 = 0xc4;
pub const MULTIANEWARRAY: u8 = 0xc5;
pub const IFNULL: u8 = 0xc6;
pub const IFNONNULL: u8 = 0xc7;
pub const GOTO_W: u8 = 0xc8;
pub const JSR_W: u8 = 0xc9;
pub const BREAKPOINT: u8 = 0xca;
pub const IMPDEP1: u8 = 0xfe;
pub const IMPDEP2: u8 = 0xff;

pub static INSTRUCTION_NAMES: &[&str] = &[
    "nop",
    "aconst_null",
    "iconst_m1",
    "iconst_0",
    "iconst_1",
    "iconst_2",
    "iconst_3",
    "iconst_4",
    "iconst_5",
    "lconst_0",
    "lconst_1",
    "fconst_0",
    "fconst_1",
    "fconst_2",
    "dconst_0",
    "dconst_1",
    "bipush",
    "sipush",
    "ldc",
    "ldc_w",
    "ldc2_w",
    "iload",
    "lload",
    "fload",
    "dload",
    "aload",
    "iload_0",
    "iload_1",
    "iload_2",
    "iload_3",
    "lload_0",
    "lload_1",
    "lload_2",
    "lload_3",
    "fload_0",
    "fload_1",
    "fload_2",
    "fload_3",
    "dload_0",
    "dload_1",
    "dload_2",
    "dload_3",
    "aload_0",
    "aload_1",
    "aload_2",
    "aload_3",
    "iaload",
    "laload",
    "faload",
    "daload",
    "aaload",
    "baload",
    "caload",
    "saload",
    "istore",
    "lstore",
    "fstore",
    "dstore",
    "astore",
    "istore_0",
    "istore_1",
    "istore_2",
    "istore_3",
    "lstore_0",
    "lstore_1",
    "lstore_2",
    "lstore_3",
    "fstore_0",
    "fstore_1",
    "fstore_2",
    "fstore_3",
    "dstore_0",
    "dstore_1",
    "dstore_2",
    "dstore_3",
    "astore_0",
    "astore_1",
    "astore_2",
    "astore_3",
    "iastore",
    "lastore",
    "fastore",
    "dastore",
    "aastore",
    "bastore",
    "castore",
    "sastore",
    "pop",
    "pop2",
    "dup",
    "dup_x1",
    "dup_x2",
    "dup2",
    "dup2_x1",
    "dup2_x2",
    "swap",
    "iadd",
    "ladd",
    "fadd",
    "dadd",
    "isub",
    "lsub",
    "fsub",
    "dsub",
    "imul",
    "lmul",
    "fmul",
    "dmul",
    "idiv",
    "ldiv",
    "fdiv",
    "ddiv",
    "irem",
    "lrem",
    "frem",
    "drem",
    "ineg",
    "lneg",
    "fneg",
    "dneg",
    "ishl",
    "lshl",
    "ishr",
    "lshr",
    "iushr",
    "lushr",
    "iand",
    "land",
    "ior",
    "lor",
    "ixor",
    "lxor",
    "iinc",
    "i2l",
    "i2f",
    "i2d",
    "l2i",
    "l2f",
    "l2d",
    "f2i",
    "f2l",
    "f2d",
    "d2i",
    "d2l",
    "d2f",
    "i2b",
    "i2c",
    "i2s",
    "lcmp",
    "fcmpl",
    "fcmpg",
    "dcmpl",
    "dcmpg",
    "ifeq",
    "ifne",
    "iflt",
    "ifge",
    "ifgt",
    "ifle",
    "if_icmpeq",
    "if_icmpne",
    "if_icmplt",
    "if_icmpge",
    "if_icmpgt",
    "if_icmple",
    "if_acmpeq",
    "if_acmpne",
    "goto",
    "jsr",
    "ret",
    "tableswitch",
    "lookupswitch",
    "ireturn",
    "lreturn",
    "freturn",
    "dreturn",
    "areturn",
    "return",
    "getstatic",
    "putstatic",
    "getfield",
    "putfield",
    "invokevirtual",
    "invokespecial",
    "invokestatic",
    "invokeinterface",
    "invokedynamic",
    "new",
    "newarray",
    "anewarray",
    "arraylength",
    "athrow",
    "checkcast",
    "instanceof",
    "monitorenter",
    "monitorexit",
    "wide",
    "multianewarray",
    "ifnull",
    "ifnonnull",
    "goto_w",
    "jsr_w",
    "breakpoint",
    /* "impdep1",
     * "impdep2", */
];

pub enum InstructionParseError {
    Parse(ParseError),
    InvalidPrimitiveType(u8),
    UnknownOpcode(u8),
}

impl From<ParseError> for InstructionParseError {
    fn from(err: ParseError) -> Self {
        InstructionParseError::Parse(err)
    }
}

pub fn parse_instruction(
    input: &mut ByteParser<'_>,
) -> Result<InstructionEntry, InstructionParseError> {
    let tag = input.parse_u8()?;
    Ok(InstructionEntry {
        start: input.offset - 1,
        tag,
        instruction: match tag {
            NOP => Instruction::Nop,

            ACONST_NULL => Instruction::ConstNull,
            ICONST_M1 => Instruction::ConstInt(-1),
            ICONST_0 => Instruction::ConstInt(0),
            ICONST_1 => Instruction::ConstInt(1),
            ICONST_2 => Instruction::ConstInt(2),
            ICONST_3 => Instruction::ConstInt(3),
            ICONST_4 => Instruction::ConstInt(4),
            ICONST_5 => Instruction::ConstInt(5),
            LCONST_0 => Instruction::ConstLong(0),
            LCONST_1 => Instruction::ConstLong(1),
            FCONST_0 => Instruction::ConstFloat(0.0),
            FCONST_1 => Instruction::ConstFloat(1.0),
            FCONST_2 => Instruction::ConstFloat(2.0),
            DCONST_0 => Instruction::ConstDouble(0.0),
            DCONST_1 => Instruction::ConstDouble(1.0),

            BIPUSH => Instruction::PushByte(input.parse_u8()?),
            SIPUSH => Instruction::PushShort(input.parse_i16()?),

            LDC => Instruction::LoadConstant(input.parse_u8()? as u16),
            LDC_W => Instruction::LoadConstant(input.parse_u16()?),
            LDC2_W => Instruction::LoadConstant(input.parse_u16()?),

            ILOAD => Instruction::LoadInt(input.parse_u8()?),
            LLOAD => Instruction::LoadLong(input.parse_u8()?),
            FLOAD => Instruction::LoadFloat(input.parse_u8()?),
            DLOAD => Instruction::LoadDouble(input.parse_u8()?),
            ALOAD => Instruction::LoadRef(input.parse_u8()?),

            ILOAD_0 => Instruction::LoadInt(0),
            ILOAD_1 => Instruction::LoadInt(1),
            ILOAD_2 => Instruction::LoadInt(2),
            ILOAD_3 => Instruction::LoadInt(3),
            LLOAD_0 => Instruction::LoadLong(0),
            LLOAD_1 => Instruction::LoadLong(1),
            LLOAD_2 => Instruction::LoadLong(2),
            LLOAD_3 => Instruction::LoadLong(3),
            FLOAD_0 => Instruction::LoadFloat(0),
            FLOAD_1 => Instruction::LoadFloat(1),
            FLOAD_2 => Instruction::LoadFloat(2),
            FLOAD_3 => Instruction::LoadFloat(3),
            DLOAD_0 => Instruction::LoadDouble(0),
            DLOAD_1 => Instruction::LoadDouble(1),
            DLOAD_2 => Instruction::LoadDouble(2),
            DLOAD_3 => Instruction::LoadDouble(3),
            ALOAD_0 => Instruction::LoadRef(0),
            ALOAD_1 => Instruction::LoadRef(1),
            ALOAD_2 => Instruction::LoadRef(2),
            ALOAD_3 => Instruction::LoadRef(3),

            IALOAD => Instruction::LoadArrayInt,
            LALOAD => Instruction::LoadArrayLong,
            FALOAD => Instruction::LoadArrayFloat,
            DALOAD => Instruction::LoadArrayDouble,
            AALOAD => Instruction::LoadArrayRef,
            BALOAD => Instruction::LoadArrayBool,
            CALOAD => Instruction::LoadArrayChar,
            SALOAD => Instruction::LoadArrayShort,

            ISTORE => Instruction::StoreInt(input.parse_u8()?),
            LSTORE => Instruction::StoreLong(input.parse_u8()?),
            FSTORE => Instruction::StoreFloat(input.parse_u8()?),
            DSTORE => Instruction::StoreDouble(input.parse_u8()?),
            ASTORE => Instruction::StoreRef(input.parse_u8()?),

            ISTORE_0 => Instruction::StoreInt(0),
            ISTORE_1 => Instruction::StoreInt(1),
            ISTORE_2 => Instruction::StoreInt(2),
            ISTORE_3 => Instruction::StoreInt(3),
            LSTORE_0 => Instruction::StoreLong(0),
            LSTORE_1 => Instruction::StoreLong(1),
            LSTORE_2 => Instruction::StoreLong(2),
            LSTORE_3 => Instruction::StoreLong(3),
            FSTORE_0 => Instruction::StoreFloat(0),
            FSTORE_1 => Instruction::StoreFloat(1),
            FSTORE_2 => Instruction::StoreFloat(2),
            FSTORE_3 => Instruction::StoreFloat(3),
            DSTORE_0 => Instruction::StoreDouble(0),
            DSTORE_1 => Instruction::StoreDouble(1),
            DSTORE_2 => Instruction::StoreDouble(2),
            DSTORE_3 => Instruction::StoreDouble(3),
            ASTORE_0 => Instruction::StoreRef(0),
            ASTORE_1 => Instruction::StoreRef(1),
            ASTORE_2 => Instruction::StoreRef(2),
            ASTORE_3 => Instruction::StoreRef(3),

            IASTORE => Instruction::StoreArrayInt,
            LASTORE => Instruction::StoreArrayLong,
            FASTORE => Instruction::StoreArrayFloat,
            DASTORE => Instruction::StoreArrayDouble,
            AASTORE => Instruction::StoreArrayRef,
            BASTORE => Instruction::StoreArrayBool,
            CASTORE => Instruction::StoreArrayChar,
            SASTORE => Instruction::StoreArrayShort,

            POP => Instruction::Pop,
            POP2 => Instruction::Pop2,
            DUP => Instruction::Dup,
            DUP_X1 => Instruction::DupX1,
            DUP_X2 => Instruction::DupX2,
            DUP2 => Instruction::Dup2,
            DUP2_X1 => Instruction::Dup2X1,
            DUP2_X2 => Instruction::Dup2X2,
            SWAP => Instruction::Swap,

            IADD => Instruction::AddInt,
            LADD => Instruction::AddLong,
            FADD => Instruction::AddFloat,
            DADD => Instruction::AddDouble,
            ISUB => Instruction::SubInt,
            LSUB => Instruction::SubLong,
            FSUB => Instruction::SubFloat,
            DSUB => Instruction::SubDouble,
            IMUL => Instruction::MulInt,
            LMUL => Instruction::MulLong,
            FMUL => Instruction::MulFloat,
            DMUL => Instruction::MulDouble,
            IDIV => Instruction::DivInt,
            LDIV => Instruction::DivLong,
            FDIV => Instruction::DivFloat,
            DDIV => Instruction::DivDouble,
            IREM => Instruction::RemInt,
            LREM => Instruction::RemLong,
            FREM => Instruction::RemFloat,
            DREM => Instruction::RemDouble,
            INEG => Instruction::NegInt,
            LNEG => Instruction::NegLong,
            FNEG => Instruction::NegFloat,
            DNEG => Instruction::NegDouble,
            ISHL => Instruction::ShlInt,
            LSHL => Instruction::ShlLong,
            ISHR => Instruction::ShrInt,
            LSHR => Instruction::ShrLong,
            IUSHR => Instruction::LogicalShrInt,
            LUSHR => Instruction::LogicalShrLong,
            IAND => Instruction::AndInt,
            LAND => Instruction::AndLong,
            IOR => Instruction::OrInt,
            LOR => Instruction::OrLong,
            IXOR => Instruction::XorInt,
            LXOR => Instruction::XorLong,
            IINC => Instruction::IncInt,

            I2L => Instruction::IntToLong,
            I2F => Instruction::IntToFloat,
            I2D => Instruction::IntToDouble,
            L2I => Instruction::LongToInt,
            L2F => Instruction::LongToFloat,
            L2D => Instruction::LongToDouble,
            F2I => Instruction::FloatToInt,
            F2L => Instruction::FloatToLong,
            F2D => Instruction::FloatToDouble,
            D2I => Instruction::DoubleToInt,
            D2L => Instruction::DoubleToLong,
            D2F => Instruction::DoubleToFloat,
            I2B => Instruction::IntToBool,
            I2C => Instruction::IntToChar,
            I2S => Instruction::IntToShort,

            LCMP => Instruction::CompareLong,
            FCMPL => Instruction::CompareLessFloat,
            FCMPG => Instruction::CompareGreaterFloat,
            DCMPL => Instruction::CompareLessDouble,
            DCMPG => Instruction::CompareGreaterDouble,
            IFEQ => Instruction::IfEqual(input.parse_i16()?),
            IFNE => Instruction::IfNotEqual(input.parse_i16()?),
            IFLT => Instruction::IfLessThan(input.parse_i16()?),
            IFGE => Instruction::IfGreaterThan(input.parse_i16()?),
            IFGT => Instruction::IfLessThanEqual(input.parse_i16()?),
            IFLE => Instruction::IfGreaterThanEqual(input.parse_i16()?),

            IF_ICMPEQ => Instruction::IfEqualInt(input.parse_i16()?),
            IF_ICMPNE => Instruction::IfNotEqualInt(input.parse_i16()?),
            IF_ICMPLT => Instruction::IfLessThanInt(input.parse_i16()?),
            IF_ICMPGE => Instruction::IfGreaterThanEqualInt(input.parse_i16()?),
            IF_ICMPGT => Instruction::IfGreaterThanInt(input.parse_i16()?),
            IF_ICMPLE => Instruction::IfLessThanEqualInt(input.parse_i16()?),
            IF_ACMPEQ => Instruction::IfEqualRef(input.parse_i16()?),
            IF_ACMPNE => Instruction::IfNotEqualRef(input.parse_i16()?),
            IFNULL => Instruction::IfNull(input.parse_i16()?),
            IFNONNULL => Instruction::IfNonNull(input.parse_i16()?),

            GOTO => Instruction::Goto(input.parse_i16()? as i32),
            JSR => Instruction::Jsr(input.parse_i16()? as i32),
            GOTO_W => Instruction::Goto(input.parse_i32()?),
            JSR_W => Instruction::Jsr(input.parse_i32()?),
            RET => Instruction::Ret(input.parse_u8()?),

            TABLESWITCH => panic!("Tableswitch"),
            LOOKUPSWITCH => panic!("Lookupswitch"),

            IRETURN => Instruction::ReturnInt,
            LRETURN => Instruction::ReturnLong,
            FRETURN => Instruction::ReturnFloat,
            DRETURN => Instruction::ReturnDouble,
            ARETURN => Instruction::ReturnRef,
            RETURN => Instruction::ReturnVoid,

            GETSTATIC => Instruction::GetStatic(input.parse_u16()?),
            PUTSTATIC => Instruction::PutStatic(input.parse_u16()?),
            GETFIELD => Instruction::GetField(input.parse_u16()?),
            PUTFIELD => Instruction::PutField(input.parse_u16()?),

            INVOKEVIRTUAL => Instruction::InvokeVirtual(input.parse_u16()?),
            INVOKESPECIAL => Instruction::InvokeSpecial(input.parse_u16()?),
            INVOKESTATIC => Instruction::InvokeStatic(input.parse_u16()?),
            INVOKEDYNAMIC => Instruction::InvokeDynamic(input.parse_u16()?),
            INVOKEINTERFACE => Instruction::InvokeInterface(input.parse_u16()?, input.parse_u8()?),

            NEW => Instruction::New(input.parse_u16()?),
            NEWARRAY => Instruction::NewArrayPrimitive(match input.parse_u8()? {
                0 => ArrayPrimitiveType::Int,
                1 => ArrayPrimitiveType::Long,
                4 => ArrayPrimitiveType::Boolean,
                5 => ArrayPrimitiveType::Char,
                6 => ArrayPrimitiveType::Float,
                7 => ArrayPrimitiveType::Double,
                8 => ArrayPrimitiveType::Byte,
                9 => ArrayPrimitiveType::Short,
                k => return Err(InstructionParseError::InvalidPrimitiveType(k)),
            }),
            ANEWARRAY => Instruction::NewArrayRef(input.parse_u16()?),

            ARRAYLENGTH => Instruction::ArrayLength,
            ATHROW => Instruction::Throw,
            CHECKCAST => Instruction::CheckCast(input.parse_u16()?),
            INSTANCEOF => Instruction::InstanceOf(input.parse_u16()?),
            MONITORENTER => Instruction::MonitorEnter,
            MONITOREXIT => Instruction::MonitorExit,
            MULTIANEWARRAY => Instruction::NewArrayMultiRef(input.parse_u16()?, input.parse_u8()?),

            WIDE => Instruction::Wide(Box::new(parse_instruction(input)?)),
            BREAKPOINT => Instruction::Breakpoint,
            IMPDEP1 => Instruction::Impdep1,
            IMPDEP2 => Instruction::Impdep2,

            other => return Err(InstructionParseError::UnknownOpcode(other)),
        },
    })
}

#[derive(Clone, Debug, PartialEq)]
pub struct InstructionEntry {
    pub start: usize,
    pub tag: u8,
    pub instruction: Instruction,
}

pub fn parse_instructions_into<F>(bytes: &[u8], mut func: F)
where
    F: FnMut(InstructionEntry),
{
    let mut parser = ByteParser::new(bytes);
    while let Ok(entry) = parse_instruction(&mut parser) {
        func(entry);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Nop,

    ConstNull,
    ConstInt(i32),
    ConstLong(i64),
    ConstFloat(f32),
    ConstDouble(f64),

    PushByte(u8),
    PushShort(i16),
    LoadConstant(u16),

    LoadInt(u8),
    LoadLong(u8),
    LoadFloat(u8),
    LoadDouble(u8),
    LoadRef(u8),

    LoadArrayInt,
    LoadArrayLong,
    LoadArrayFloat,
    LoadArrayDouble,
    LoadArrayRef,
    LoadArrayBool,
    LoadArrayChar,
    LoadArrayShort,

    StoreInt(u8),
    StoreLong(u8),
    StoreFloat(u8),
    StoreDouble(u8),
    StoreRef(u8),

    StoreArrayInt,
    StoreArrayLong,
    StoreArrayFloat,
    StoreArrayDouble,
    StoreArrayRef,
    StoreArrayBool,
    StoreArrayChar,
    StoreArrayShort,

    Pop,
    Pop2,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Swap,

    AddInt,
    AddLong,
    AddFloat,
    AddDouble,
    SubInt,
    SubLong,
    SubFloat,
    SubDouble,
    MulInt,
    MulLong,
    MulFloat,
    MulDouble,
    DivInt,
    DivLong,
    DivFloat,
    DivDouble,
    RemInt,
    RemLong,
    RemFloat,
    RemDouble,
    NegInt,
    NegLong,
    NegFloat,
    NegDouble,
    ShlInt,
    ShlLong,
    ShrInt,
    ShrLong,
    LogicalShrInt,
    LogicalShrLong,
    AndInt,
    AndLong,
    OrInt,
    OrLong,
    XorInt,
    XorLong,
    IncInt,

    IntToLong,
    IntToFloat,
    IntToDouble,
    LongToInt,
    LongToFloat,
    LongToDouble,
    FloatToInt,
    FloatToLong,
    FloatToDouble,
    DoubleToInt,
    DoubleToLong,
    DoubleToFloat,
    IntToBool,
    IntToChar,
    IntToShort,

    CompareLong,
    CompareLessFloat,
    CompareGreaterFloat,
    CompareLessDouble,
    CompareGreaterDouble,

    IfEqual(i16),
    IfNotEqual(i16),
    IfLessThan(i16),
    IfGreaterThan(i16),
    IfLessThanEqual(i16),
    IfGreaterThanEqual(i16),

    IfEqualInt(i16),
    IfNotEqualInt(i16),
    IfLessThanInt(i16),
    IfGreaterThanEqualInt(i16),
    IfGreaterThanInt(i16),
    IfLessThanEqualInt(i16),

    IfEqualRef(i16),
    IfNotEqualRef(i16),
    IfNull(i16),
    IfNonNull(i16),

    Goto(i32),
    Jsr(i32),
    Ret(u8),

    // A tableswitch is a variable-length instruction. Immediately after the tableswitch opcode,
    // between zero and three bytes must act as padding, such that defaultbyte1 begins at an
    // address that is a multiple of four bytes from the start of the current method (the opcode
    // of its first instruction). Immediately after the padding are bytes constituting three
    // signed 32-bit values: default, low, and high. Immediately following are bytes constituting
    // a series of high - low + 1 signed 32-bit offsets. The value low must be less than or equal
    // to high. The high - low + 1 signed 32-bit offsets are treated as a 0-based jump table. Each
    // of these signed 32-bit values is constructed as (byte1 << 24) | (byte2 << 16) | (byte3 <<
    // 8) | byte4.

    // The index must be of type int and is popped from the operand stack. If index is less than
    // low or index is greater than high, then a target address is calculated by adding default to
    // the address of the opcode of this tableswitch instruction. Otherwise, the offset at
    // position index - low of the jump table is extracted. The target address is calculated by
    // adding that offset to the address of the opcode of this tableswitch instruction. Execution
    // then continues at the target address.

    // The target address that can be calculated from each jump table offset, as well as the one
    // that can be calculated from default, must be the address of an opcode of an instruction
    // within the method that contains this tableswitch instruction.
    Tableswitch,

    // A lookupswitch is a variable-length instruction. Immediately after the lookupswitch
    // opcode, between zero and three bytes must act as padding, such that defaultbyte1 begins at
    // an address that is a multiple of four bytes from the start of the current method (the
    // opcode of its first instruction). Immediately after the padding follow a series of signed
    // 32-bit values: default, npairs, and then npairs pairs of signed 32-bit values. The npairs
    // must be greater than or equal to 0. Each of the npairs pairs consists of an int match and a
    // signed 32-bit offset. Each of these signed 32-bit values is constructed from four unsigned
    // bytes as (byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4.

    // The table match-offset pairs of the lookupswitch instruction must be sorted in increasing
    // numerical order by match.

    // The key must be of type int and is popped from the operand stack. The key is compared
    // against the match values. If it is equal to one of them, then a target address is
    // calculated by adding the corresponding offset to the address of the opcode of this
    // lookupswitch instruction. If the key does not match any of the match values, the target
    // address is calculated by adding default to the address of the opcode of this lookupswitch
    // instruction. Execution then continues at the target address.

    // The target address that can be calculated from the offset of each match-offset pair, as
    // well as the one calculated from default, must be the address of an opcode of an instruction
    // within the method that contains this lookupswitch instruction.
    Lookupswitch,

    ReturnInt,
    ReturnLong,
    ReturnFloat,
    ReturnDouble,
    ReturnRef,
    ReturnVoid,

    GetStatic(u16),
    PutStatic(u16),
    GetField(u16),
    PutField(u16),

    InvokeVirtual(u16),
    InvokeSpecial(u16),
    InvokeStatic(u16),
    InvokeInterface(u16, u8),
    InvokeDynamic(u16),

    New(u16),
    NewArrayPrimitive(ArrayPrimitiveType),
    NewArrayRef(u16),
    NewArrayMultiRef(u16, u8),

    ArrayLength,
    Throw,
    CheckCast(u16),
    InstanceOf(u16),
    MonitorEnter,
    MonitorExit,
    Wide(Box<InstructionEntry>),

    Breakpoint,
    Impdep1,
    Impdep2,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ArrayPrimitiveType {
    Boolean = 4,
    Char = 5,
    Float = 6,
    Double = 7,
    Byte = 8,
    Short = 9,
    Int = 0,
    Long = 1,
}
