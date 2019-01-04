use class::parse::{ByteParser, ParseError};

pub fn instruction_name(tag: u8) -> &'static str {
    match tag {
        BREAKPOINT => "breakpoint",
        IMPDEP1 => "impdep1",
        IMPDEP2 => "impdep2",
        _ => INSTRUCTION_NAMES
            .get(tag as usize)
            .cloned()
            .unwrap_or("<unknown>"),
    }
}

macro_rules! instructions {
    ($input:ident; $($opcode:expr => $name:ident, $display:expr, $parse:expr;)*) => {
        $(pub const $name: u8 = $opcode;)*

        pub const BREAKPOINT: u8 = 0xca;
        pub const IMPDEP1: u8 = 0xfe;
        pub const IMPDEP2: u8 = 0xff;

        /// Map of instruction tag -> instruction name.
        pub static INSTRUCTION_NAMES: &[&str] = &[
            $($display,)*
        ];

        pub fn parse_instruction(
            $input: &mut ByteParser<'_>,
        ) -> Result<InstructionEntry, InstructionParseError> {
            use self::Instruction::*;

            let tag = $input.parse_u8()?;
            Ok(InstructionEntry {
                start: $input.offset - 1,
                tag,
                instruction: match tag {
                    $($name => $parse,)*

                    BREAKPOINT => Breakpoint,
                    IMPDEP1 => Impdep1,
                    IMPDEP2 => Impdep2,

                    other => return Err(InstructionParseError::UnknownOpcode(other)),
                }
            })
        }
    };
}

instructions! {
    input;

    0x00 => NOP, "nop", Nop;
    0x01 => ACONST_NULL, "aconst_null", ConstNull;
    0x02 => ICONST_M1, "iconst_m1", ConstInt(-1);
    0x03 => ICONST_0, "iconst_0", ConstInt(0);
    0x04 => ICONST_1, "iconst_1", ConstInt(1);
    0x05 => ICONST_2, "iconst_2", ConstInt(2);
    0x06 => ICONST_3, "iconst_3", ConstInt(3);
    0x07 => ICONST_4, "iconst_4", ConstInt(4);
    0x08 => ICONST_5, "iconst_5", ConstInt(5);
    0x09 => LCONST_0, "lconst_0", ConstLong(0);
    0x0a => LCONST_1, "lconst_1", ConstLong(1);
    0x0b => FCONST_0, "fconst_0", ConstFloat(0.0);
    0x0c => FCONST_1, "fconst_1", ConstFloat(1.0);
    0x0d => FCONST_2, "fconst_2", ConstFloat(2.0);
    0x0e => DCONST_0, "dconst_0", ConstDouble(0.0);
    0x0f => DCONST_1, "dconst_1", ConstDouble(1.0);
    0x10 => BIPUSH, "bipush", PushByte(input.parse_u8()?);
    0x11 => SIPUSH, "sipush", PushShort(input.parse_i16()?);
    0x12 => LDC, "ldc", LoadConstant(input.parse_u8()? as u16);
    0x13 => LDC_W, "ldc_w", LoadConstant(input.parse_u16()?);
    0x14 => LDC2_W, "ldc2_w", LoadConstant(input.parse_u16()?);
    0x15 => ILOAD, "iload", LoadInt(input.parse_u8()?);
    0x16 => LLOAD, "lload", LoadLong(input.parse_u8()?);
    0x17 => FLOAD, "fload", LoadFloat(input.parse_u8()?);
    0x18 => DLOAD, "dload", LoadDouble(input.parse_u8()?);
    0x19 => ALOAD, "aload", LoadRef(input.parse_u8()?);
    0x1a => ILOAD_0, "iload_0", LoadInt(0);
    0x1b => ILOAD_1, "iload_1", LoadInt(1);
    0x1c => ILOAD_2, "iload_2", LoadInt(2);
    0x1d => ILOAD_3, "iload_3", LoadInt(3);
    0x1e => LLOAD_0, "lload_0", LoadLong(0);
    0x1f => LLOAD_1, "lload_1", LoadLong(1);
    0x20 => LLOAD_2, "lload_2", LoadLong(2);
    0x21 => LLOAD_3, "lload_3", LoadLong(3);
    0x22 => FLOAD_0, "fload_0", LoadFloat(0);
    0x23 => FLOAD_1, "fload_1", LoadFloat(1);
    0x24 => FLOAD_2, "fload_2", LoadFloat(2);
    0x25 => FLOAD_3, "fload_3", LoadFloat(3);
    0x26 => DLOAD_0, "dload_0", LoadDouble(0);
    0x27 => DLOAD_1, "dload_1", LoadDouble(1);
    0x28 => DLOAD_2, "dload_2", LoadDouble(2);
    0x29 => DLOAD_3, "dload_3", LoadDouble(3);
    0x2a => ALOAD_0, "aload_0", LoadRef(0);
    0x2b => ALOAD_1, "aload_1", LoadRef(1);
    0x2c => ALOAD_2, "aload_2", LoadRef(2);
    0x2d => ALOAD_3, "aload_3", LoadRef(3);
    0x2e => IALOAD, "iaload", LoadArrayInt;
    0x2f => LALOAD, "laload", LoadArrayLong;
    0x30 => FALOAD, "faload", LoadArrayFloat;
    0x31 => DALOAD, "daload", LoadArrayDouble;
    0x32 => AALOAD, "aaload", LoadArrayRef;
    0x33 => BALOAD, "baload", LoadArrayBool;
    0x34 => CALOAD, "caload", LoadArrayChar;
    0x35 => SALOAD, "saload", LoadArrayShort;
    0x36 => ISTORE, "istore", StoreInt(input.parse_u8()?);
    0x37 => LSTORE, "lstore", StoreLong(input.parse_u8()?);
    0x38 => FSTORE, "fstore", StoreFloat(input.parse_u8()?);
    0x39 => DSTORE, "dstore", StoreDouble(input.parse_u8()?);
    0x3a => ASTORE, "astore", StoreRef(input.parse_u8()?);
    0x3b => ISTORE_0, "istore_0", StoreInt(0);
    0x3c => ISTORE_1, "istore_1", StoreInt(1);
    0x3d => ISTORE_2, "istore_2", StoreInt(2);
    0x3e => ISTORE_3, "istore_3", StoreInt(3);
    0x3f => LSTORE_0, "lstore_0", StoreLong(0);
    0x40 => LSTORE_1, "lstore_1", StoreLong(1);
    0x41 => LSTORE_2, "lstore_2", StoreLong(2);
    0x42 => LSTORE_3, "lstore_3", StoreLong(3);
    0x43 => FSTORE_0, "fstore_0", StoreFloat(0);
    0x44 => FSTORE_1, "fstore_1", StoreFloat(1);
    0x45 => FSTORE_2, "fstore_2", StoreFloat(2);
    0x46 => FSTORE_3, "fstore_3", StoreFloat(3);
    0x47 => DSTORE_0, "dstore_0", StoreDouble(0);
    0x48 => DSTORE_1, "dstore_1", StoreDouble(1);
    0x49 => DSTORE_2, "dstore_2", StoreDouble(2);
    0x4a => DSTORE_3, "dstore_3", StoreDouble(3);
    0x4b => ASTORE_0, "astore_0", StoreRef(0);
    0x4c => ASTORE_1, "astore_1", StoreRef(1);
    0x4d => ASTORE_2, "astore_2", StoreRef(2);
    0x4e => ASTORE_3, "astore_3", StoreRef(3);
    0x4f => IASTORE, "iastore", StoreArrayInt;
    0x50 => LASTORE, "lastore", StoreArrayLong;
    0x51 => FASTORE, "fastore", StoreArrayFloat;
    0x52 => DASTORE, "dastore", StoreArrayDouble;
    0x53 => AASTORE, "aastore", StoreArrayRef;
    0x54 => BASTORE, "bastore", StoreArrayBool;
    0x55 => CASTORE, "castore", StoreArrayChar;
    0x56 => SASTORE, "sastore", StoreArrayShort;
    0x57 => POP, "pop", Pop;
    0x58 => POP2, "pop2", Pop2;
    0x59 => DUP, "dup", Dup;
    0x5a => DUP_X1, "dup_x1", DupX1;
    0x5b => DUP_X2, "dup_x2", DupX2;
    0x5c => DUP2, "dup2", Dup2;
    0x5d => DUP2_X1, "dup2_x1", Dup2X1;
    0x5e => DUP2_X2, "dup2_x2", Dup2X2;
    0x5f => SWAP, "swap", Swap;
    0x60 => IADD, "iadd", AddInt;
    0x61 => LADD, "ladd", AddLong;
    0x62 => FADD, "fadd", AddFloat;
    0x63 => DADD, "dadd", AddDouble;
    0x64 => ISUB, "isub", SubInt;
    0x65 => LSUB, "lsub", SubLong;
    0x66 => FSUB, "fsub", SubFloat;
    0x67 => DSUB, "dsub", SubDouble;
    0x68 => IMUL, "imul", MulInt;
    0x69 => LMUL, "lmul", MulLong;
    0x6a => FMUL, "fmul", MulFloat;
    0x6b => DMUL, "dmul", MulDouble;
    0x6c => IDIV, "idiv", DivInt;
    0x6d => LDIV, "ldiv", DivLong;
    0x6e => FDIV, "fdiv", DivFloat;
    0x6f => DDIV, "ddiv", DivDouble;
    0x70 => IREM, "irem", RemInt;
    0x71 => LREM, "lrem", RemLong;
    0x72 => FREM, "frem", RemFloat;
    0x73 => DREM, "drem", RemDouble;
    0x74 => INEG, "ineg", NegInt;
    0x75 => LNEG, "lneg", NegLong;
    0x76 => FNEG, "fneg", NegFloat;
    0x77 => DNEG, "dneg", NegDouble;
    0x78 => ISHL, "ishl", ShlInt;
    0x79 => LSHL, "lshl", ShlLong;
    0x7a => ISHR, "ishr", ShrInt;
    0x7b => LSHR, "lshr", ShrLong;
    0x7c => IUSHR, "iushr", LogicalShrInt;
    0x7d => LUSHR, "lushr", LogicalShrLong;
    0x7e => IAND, "iand", AndInt;
    0x7f => LAND, "land", AndLong;
    0x80 => IOR, "ior", OrInt;
    0x81 => LOR, "lor", OrLong;
    0x82 => IXOR, "ixor", XorInt;
    0x83 => LXOR, "lxor", XorLong;
    0x84 => IINC, "iinc", IncInt;
    0x85 => I2L, "i2l", IntToLong;
    0x86 => I2F, "i2f", IntToFloat;
    0x87 => I2D, "i2d", IntToDouble;
    0x88 => L2I, "l2i", LongToInt;
    0x89 => L2F, "l2f", LongToFloat;
    0x8a => L2D, "l2d", LongToDouble;
    0x8b => F2I, "f2i", FloatToInt;
    0x8c => F2L, "f2l", FloatToLong;
    0x8d => F2D, "f2d", FloatToDouble;
    0x8e => D2I, "d2i", DoubleToInt;
    0x8f => D2L, "d2l", DoubleToLong;
    0x90 => D2F, "d2f", DoubleToFloat;
    0x91 => I2B, "i2b", IntToBool;
    0x92 => I2C, "i2c", IntToChar;
    0x93 => I2S, "i2s", IntToShort;
    0x94 => LCMP, "lcmp", CompareLong;
    0x95 => FCMPL, "fcmpl", CompareLessFloat;
    0x96 => FCMPG, "fcmpg", CompareGreaterFloat;
    0x97 => DCMPL, "dcmpl", CompareLessDouble;
    0x98 => DCMPG, "dcmpg", CompareGreaterDouble;
    0x99 => IFEQ, "ifeq", IfEqual(input.parse_i16()?);
    0x9a => IFNE, "ifne", IfNotEqual(input.parse_i16()?);
    0x9b => IFLT, "iflt", IfLessThan(input.parse_i16()?);
    0x9c => IFGE, "ifge", IfGreaterThan(input.parse_i16()?);
    0x9d => IFGT, "ifgt", IfLessThanEqual(input.parse_i16()?);
    0x9e => IFLE, "ifle", IfGreaterThanEqual(input.parse_i16()?);
    0x9f => IF_ICMPEQ, "if_icmpeq", IfEqualInt(input.parse_i16()?);
    0xa0 => IF_ICMPNE, "if_icmpne", IfNotEqualInt(input.parse_i16()?);
    0xa1 => IF_ICMPLT, "if_icmplt", IfLessThanInt(input.parse_i16()?);
    0xa2 => IF_ICMPGE, "if_icmpge", IfGreaterThanEqualInt(input.parse_i16()?);
    0xa3 => IF_ICMPGT, "if_icmpgt", IfGreaterThanInt(input.parse_i16()?);
    0xa4 => IF_ICMPLE, "if_icmple", IfLessThanEqualInt(input.parse_i16()?);
    0xa5 => IF_ACMPEQ, "if_acmpeq", IfEqualRef(input.parse_i16()?);
    0xa6 => IF_ACMPNE, "if_acmpne", IfNotEqualRef(input.parse_i16()?);
    0xa7 => GOTO, "goto", Goto(input.parse_i16()? as i32);
    0xa8 => JSR, "jsr", Jsr(input.parse_i16()? as i32);
    0xa9 => RET, "ret", Ret(input.parse_u8()?);
    0xaa => TABLESWITCH, "tableswitch", panic!("tableswitch");
    0xab => LOOKUPSWITCH, "lookupswitch", panic!("lookupswitch");
    0xac => IRETURN, "ireturn", ReturnInt;
    0xad => LRETURN, "lreturn", ReturnLong;
    0xae => FRETURN, "freturn", ReturnFloat;
    0xaf => DRETURN, "dreturn", ReturnDouble;
    0xb0 => ARETURN, "areturn", ReturnRef;
    0xb1 => RETURN, "return", ReturnVoid;
    0xb2 => GETSTATIC, "getstatic", GetStatic(input.parse_u16()?);
    0xb3 => PUTSTATIC, "putstatic", PutStatic(input.parse_u16()?);
    0xb4 => GETFIELD, "getfield", GetField(input.parse_u16()?);
    0xb5 => PUTFIELD, "putfield", PutField(input.parse_u16()?);
    0xb6 => INVOKEVIRTUAL, "invokevirtual", InvokeVirtual(input.parse_u16()?);
    0xb7 => INVOKESPECIAL, "invokespecial", InvokeSpecial(input.parse_u16()?);
    0xb8 => INVOKESTATIC, "invokestatic", InvokeStatic(input.parse_u16()?);
    0xb9 => INVOKEINTERFACE, "invokeinterface", InvokeDynamic(input.parse_u16()?);
    0xba => INVOKEDYNAMIC, "invokedynamic", InvokeInterface(input.parse_u16()?, input.parse_u8()?);
    0xbb => NEW, "new", New(input.parse_u16()?);
    0xbc => NEWARRAY, "newarray", NewArrayPrimitive(match input.parse_u8()? {
        4 => ArrayPrimitiveType::Boolean,
        5 => ArrayPrimitiveType::Char,
        6 => ArrayPrimitiveType::Float,
        7 => ArrayPrimitiveType::Double,
        8 => ArrayPrimitiveType::Byte,
        9 => ArrayPrimitiveType::Short,
        10 => ArrayPrimitiveType::Int,
        11 => ArrayPrimitiveType::Long,
        k => return Err(InstructionParseError::InvalidPrimitiveType(k)),
    });
    0xbd => ANEWARRAY, "anewarray", NewArrayRef(input.parse_u16()?);
    0xbe => ARRAYLENGTH, "arraylength", ArrayLength;
    0xbf => ATHROW, "athrow", Throw;
    0xc0 => CHECKCAST, "checkcast", CheckCast(input.parse_u16()?);
    0xc1 => INSTANCEOF, "instanceof", InstanceOf(input.parse_u16()?);
    0xc2 => MONITORENTER, "monitorenter", MonitorEnter;
    0xc3 => MONITOREXIT, "monitorexit", MonitorExit;
    0xc4 => WIDE, "wide", Wide(Box::new(parse_instruction(input)?));
    0xc5 => MULTIANEWARRAY, "multianewarray", NewArrayMultiRef(input.parse_u16()?, input.parse_u8()?);
    0xc6 => IFNULL, "ifnull", IfNull(input.parse_i16()?);
    0xc7 => IFNONNULL, "ifnonnull", IfNonNull(input.parse_i16()?);
    0xc8 => GOTO_W, "goto_w", Goto(input.parse_i32()?);
    0xc9 => JSR_W, "jsr_w", Jsr(input.parse_i32()?);
    // 0xca => BREAKPOINT, "breakpoint";
    // 0xfe => IMPDEP1, "impdep1";
    // 0xff => IMPDEP2, "impdep2";
}

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
