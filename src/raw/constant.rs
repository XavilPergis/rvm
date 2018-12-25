use crate::raw::{ByteParser, *};

pub type PoolIndex = usize;

pub const CONSTANT_UTF8: u8 = 1;
pub const CONSTANT_INTEGER: u8 = 3;
pub const CONSTANT_FLOAT: u8 = 4;
pub const CONSTANT_LONG: u8 = 5;
pub const CONSTANT_DOUBLE: u8 = 6;
pub const CONSTANT_CLASS: u8 = 7;
pub const CONSTANT_STRING: u8 = 8;
pub const CONSTANT_FIELD_REF: u8 = 9;
pub const CONSTANT_METHOD_REF: u8 = 10;
pub const CONSTANT_INTERFACE_METHOD_REF: u8 = 11;
pub const CONSTANT_NAME_AND_TYPE: u8 = 12;
pub const CONSTANT_METHOD_HANDLE: u8 = 15;
pub const CONSTANT_METHOD_TYPE: u8 = 16;
pub const CONSTANT_INVOKE_DYNAMIC: u8 = 18;

// Symbolic references by an instruction sequence to fields or methods are
// indicated by C.x:T, where x and T are the name and descriptor (§4.3.2,
// §4.3.3) of the field or method, and C is the class or interface in which the
// field or method is to be found.
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum MethodHandleKind {
    /// getfield C.f:T
    GetField = 1,
    /// getstatic C.f:T
    GetStatic = 2,
    /// putfield C.f:T
    PutField = 3,
    /// putstatic C.f:T
    PutStatic = 4,
    /// invokevirtual C.m:(A*)T
    InvokeVirtual = 5,
    /// invokestatic C.m:(A*)T
    InvokeStatic = 6,
    /// invokespecial C.m:(A*)T
    InvokeSpecial = 7,
    /// new C; dup; invokespecial C.<init>:(A*)void
    NewInvokeSpecial = 8,
    /// invokeinterface C.m:(A*)T
    InvokeInterface = 9,
}

impl MethodHandleKind {
    fn from(ty: u8) -> ClassResult<MethodHandleKind> {
        Ok(match ty {
            1 => MethodHandleKind::GetField,
            2 => MethodHandleKind::GetStatic,
            3 => MethodHandleKind::PutField,
            4 => MethodHandleKind::PutStatic,
            5 => MethodHandleKind::InvokeVirtual,
            6 => MethodHandleKind::InvokeStatic,
            7 => MethodHandleKind::InvokeSpecial,
            8 => MethodHandleKind::NewInvokeSpecial,
            9 => MethodHandleKind::InvokeInterface,
            other => return Err(ClassError::UnknownMethodHandleType(other)),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    /// Just here to occupy the space that would be occupied by the upper half
    /// of a 64-bit constant table entry. It is not part of the spec, and an
    /// implementation detail.
    Nothing,

    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    String(PoolIndex),

    StringData(Box<[u8]>),

    Class(PoolIndex),
    MethodType(PoolIndex),
    FieldRef {
        class: PoolIndex,
        name_and_type: PoolIndex,
    },
    MethodRef {
        class: PoolIndex,
        name_and_type: PoolIndex,
    },
    InterfaceMethodRef {
        class: PoolIndex,
        name_and_type: PoolIndex,
    },
    NameAndType {
        name: PoolIndex,
        ty: PoolIndex,
    },

    MethodHandle {
        kind: MethodHandleKind,
        index: PoolIndex,
    },

    InvokeDynamic {
        bootstrap_method_attr: PoolIndex,
        name_and_type: PoolIndex,
    },
}

impl Constant {
    pub fn is_nothing(&self) -> bool {
        self == &Constant::Nothing
    }

    pub fn as_string_data(&self) -> Option<&[u8]> {
        match self {
            Constant::StringData(data) => Some(&**data),
            _ => None,
        }
    }
}

fn parse_constant<'src>(input: &mut ByteParser<'src>) -> ClassResult<Constant> {
    Ok(match input.parse_u8()? {
        CONSTANT_UTF8 => {
            let len = input.parse_u16()? as usize;
            Constant::StringData(input.take(len)?.into())
        }
        CONSTANT_INTEGER => input.parse_i32().map(Constant::Integer)?,
        CONSTANT_FLOAT => input.parse_f32().map(Constant::Float)?,
        CONSTANT_LONG => input.parse_i64().map(Constant::Long)?,
        CONSTANT_DOUBLE => input.parse_f64().map(Constant::Double)?,

        CONSTANT_CLASS => input.parse_u16().map(|x| Constant::Class(x as usize - 1))?,
        CONSTANT_STRING => input
            .parse_u16()
            .map(|x| Constant::String(x as usize - 1))?,
        CONSTANT_METHOD_TYPE => input
            .parse_u16()
            .map(|x| Constant::MethodType(x as usize - 1))?,

        CONSTANT_FIELD_REF => Constant::FieldRef {
            class: input.parse_u16()? as usize - 1,
            name_and_type: input.parse_u16()? as usize - 1,
        },

        CONSTANT_METHOD_REF => Constant::MethodRef {
            class: input.parse_u16()? as usize - 1,
            name_and_type: input.parse_u16()? as usize - 1,
        },

        CONSTANT_INTERFACE_METHOD_REF => Constant::InterfaceMethodRef {
            class: input.parse_u16()? as usize - 1,
            name_and_type: input.parse_u16()? as usize - 1,
        },

        CONSTANT_NAME_AND_TYPE => Constant::NameAndType {
            name: input.parse_u16()? as usize - 1,
            ty: input.parse_u16()? as usize - 1,
        },

        CONSTANT_METHOD_HANDLE => Constant::MethodHandle {
            kind: MethodHandleKind::from(input.parse_u8()?)?,
            index: input.parse_u16()? as usize - 1,
        },

        CONSTANT_INVOKE_DYNAMIC => Constant::InvokeDynamic {
            bootstrap_method_attr: input.parse_u16()? as usize,
            name_and_type: input.parse_u16()? as usize - 1,
        },

        other => return Err(ClassError::UnknownConstantTag(other)),
    })
}

pub(crate) fn parse_constant_pool<'src>(
    input: &mut ByteParser<'src>,
) -> ClassResult<Box<[Constant]>> {
    let num_consts = match input.parse_u16()? as usize {
        0 => return Err(ClassError::ConstantPoolTooSmall),
        num => num - 1,
    };

    let mut consts = Vec::with_capacity(num_consts);
    let mut cur = 0;
    while cur < num_consts {
        let constant = parse_constant(input)?;
        let is_64_bit = match constant {
            Constant::Long(_) | Constant::Double(_) => true,
            _ => false,
        };

        cur += 1;
        consts.push(constant);
        if is_64_bit {
            cur += 1;
            consts.push(Constant::Nothing);
        }
    }

    Ok(consts.into())
}
