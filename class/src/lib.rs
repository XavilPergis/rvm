pub mod attribute;
pub mod class;
pub mod constant;
pub mod field;
pub mod method;
pub(crate) mod mutf8;
pub mod parse;
pub mod signature;

use crate::parse::ParseError;

pub type ClassResult<T> = Result<T, ClassError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ClassError {
    Parse(ParseError),

    // Constant pool parsing errors
    UnknownConstantTag(u8),
    UnknownMethodHandleType(u8),
    ConstantPoolTooSmall,

    // Class parse errors
    WrongMagic,

    // Descriptor parse errors
    BadDescriptorType(u8),

    // Attribute parse errors
    UnknownVerificationType(u8),
    UnknownStackMapFrameType(u8),

    // General errors
    InvalidPoolIndex,
    InvalidPoolType,

    InvalidModifiedUtf8Byte(usize, u8),

    // Signature parse errors
    InvalidWildcardBound(u8),
    InvalidBaseType(u8),
}

impl From<ParseError> for ClassError {
    fn from(err: ParseError) -> Self {
        ClassError::Parse(err)
    }
}
