//! # Attribute Formats
//!
//! ## General Form
//! The general form of an attribute is listed below. It is comprised of an
//! index into the constant pool, representing the name of the attribute,
//! followed by the beyte length of the *rest* of the attribute, not including
//! the first six bytes, followed by attribute-specific information, whose
//! length is decided by the attrinute type.
//!
//! ```
//! AttributeInfo {
//!     name_index: u16,
//!     length: u32,
//!     info: [u8; length],
//! }
//! ```
//!
//! ## Defined Attributes
//! Certain attributes are defined by the specification to appear, and these
//! have special semantics that modify the behavior of the JVM. Any additional
//! attribute needs to be read, but cannot affect the behavior of the JVM and as
//! such are informational.
//!
//! ```
//! Attribute::ConstantValue {
//!     // must point to a `Constant::StringData` containing `"ConstantValue"`
//!     name_index: u16,
//!     // must have the value `2`
//!     length: u32,
//!     index: u16,
//! }
//!
//! // Represents an exception handler
//! // The exception handler is active between pc values of [start_pc, end_pc)
//! ExceptionInfo {
//!     start_pc: u16,
//!     end_pc: u16,
//!     handler_pc: u16,
//!     catch_type: u16,
//! }
//!
//! Attribute::Code {
//!     // must point to a `Constant::StringData` containing `"Code"`
//!     name_index: u16,
//!     length: u32,
//!     max_stack: u16,
//!     max_locals: u16,
//!     // Must be greater than 0
//!     code_length: u32,
//!     code: [u8; code_length],
//!     exceptions_length: u16,
//!     exceptions: [ExceptionInfo; exception_table_length],
//!     attributes_count: u16,
//!     attributes: [AttributeInfo; attributes_count],
//! }
//!
//! Attribute::StackMapTable {
//!     // must point to a `Constant::StringData` containing `"StackMapTable"`
//!     name_index: u16,
//!     length: u32,
//!     entries_count: u16,
//!     entries: [StackMapFrame; entries_count],
//! }
//!
//! VerificationType {
//!     tag: u8,
//!     data: match tag {
//!         0 => Top,
//!         1 => Integer,
//!         2 => Float,
//!         3 => Double,
//!         4 => Long,
//!         5 => Null,
//!         6 => UninitializedThis,
//!         7 => Object {
//!             index: u16,
//!         },
//!         8 => Uninitialized {
//!             offset: u16,
//!         },
//!     }
//! }
//!
//! StackMapFrame {
//!     frame_type: u8,
//!     frame: match frame_type {
//!         0..=63 => StackMapFrame::Same {}
//!         64..=127 => StackMapFrame::SameLocalsOneItem {
//!             stack: VerificationType
//!         }
//!         247 => StackMapFrame::SameLocalsOneItemExtended {
//!             offset_delta: u16
//!             stack: VerificationType
//!         }
//!         248..=250 => StackMapFrame::Chop {
//!             offset_delta: u16
//!         }
//!         251 => StackMapFrame::SameExtended {
//!             offset_delta: u16
//!         }
//!         252..=254 => StackMapFrame::Append {
//!             offset_delta: u16
//!             locals: [VerificationType; frame_type - 251]
//!         }
//!         255 => StackMapFrame::Full {
//!             offset_delta: u16
//!             locals_count: u16
//!             locals: [VerificationType; locals_count]
//!             stack_count: u16
//!             stack: [VerificationType; stack_count]
//!         }
//!     },
//! }
//! ```

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum VerificationType {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Uninitialized(usize),
    Object(PoolIndex),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum StackMapFrame {
    Same,
    Chop(usize),
    SameExtended(usize),
    SameLocalsOneItem(VerificationType),
    SameLocalsOneItemExtended {
        delta: usize,
        stack: VerificationType,
    },
    Append {
        delta: usize,
        locals: Box<[VerificationType]>,
    },
    Full {
        delta: usize,
        locals: Box<[VerificationType]>,
        stack: Box<[VerificationType]>,
    },
}

use crate::raw::{
    constant::{Constant, PoolIndex},
    ByteParser, ClassError, ClassResult,
};
use std::ops::Range;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExceptionInfo {
    pub active_region: Range<usize>,
    pub handler_pc: PoolIndex,
    pub catch_type: PoolIndex,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Code {
    /// The maximum amount of items on the operand stack
    pub max_stack: usize,
    /// The maximum amount of local variables, including method paramaters. The
    /// greatest local variable index is `max_locals - 1` for all types except
    /// `long` or `double`.
    pub max_locals: usize,

    /// The actual JVM bytecode for this method
    pub code: Box<[u8]>,
    /// A list of exception handlers for this method.
    pub exceptions: Box<[ExceptionInfo]>,
    /// A list of attributes for this attribute. These recognized attributes
    /// for the `Code` attribute are `LineNumberTable`, `LocalVariableTable`,
    /// `LocalvariableTypeTable`, and `StackMapTable`.
    pub attributes: Box<[AttributeInfo]>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Attribute {
    /// Attribute that was unrecognized by the class file parser, it does not
    /// have any semantic meaning and is just there as additional info
    Other(Box<[u8]>),

    /// Represents the value of a constant field. There can oly be one of these
    /// per set of attributes. If this attribute is not on a static field, then
    /// it must be ignored. Below is a table describing what constant pool entry
    /// type is associated with each field type.
    ///
    /// | Field type                      | Constant pool entry type |
    /// |:-------------------------------:|:------------------------:|
    /// | long                            | Long                     |
    /// | float                           | Float                    |
    /// | double                          | Double                   |
    /// | int, short, char, byte, boolean | Integer                  |
    /// | String                          | String                   |
    ConstantValue(PoolIndex),

    /// Contains JVM bytecode for a single method, along with additional
    /// information. This information includes the size of the code and
    /// stack, as well as exception handlers and other attributes. Every method
    /// not marked as `native` or `abstract` must have this attribute, otherwise
    /// it must *not* have this atribute. This attribute must appear at most
    /// once per method info.
    Code(Code),

    /// Contains information pertinent to the bytecode verifier, so that it can
    /// do a one-pass analysis of the bytecode and make sure that everything
    /// makes sense.
    StackMapTable(Box<[StackMapFrame]>),
}

// ExceptionInfo {
//     start_pc: u16,
//     end_pc: u16,
//     handler_pc: u16,
//     catch_type: u16,
// }
fn parse_exception_info(input: &mut ByteParser<'_>) -> ClassResult<ExceptionInfo> {
    let start = input.parse_u16()? as usize;
    let end = input.parse_u16()? as usize;
    let handler_pc = input.parse_u16()? as usize;
    let catch_type = input.parse_u16()? as usize;

    Ok(ExceptionInfo {
        active_region: start..end,
        handler_pc,
        catch_type,
    })
}

// Attribute::Code {
//     // must point to a `Constant::StringData` containing `"Code"`
//     name_index: u16,
//     length: u32,
//     max_stack: u16,
//     max_locals: u16,
//     // Must be greater than 0
//     code_length: u32,
//     code: [u8; code_length],
//     exceptions_length: u16,
//     exceptions: [ExceptionInfo; exception_table_length],
//     attributes_count: u16,
//     attributes: [AttributeInfo; attributes_count],
// }
fn parse_code(input: &mut ByteParser<'_>, pool: &[Constant]) -> ClassResult<Code> {
    let max_stack = input.parse_u16()? as usize;
    let max_locals = input.parse_u16()? as usize;
    let code_length = input.parse_u32()? as usize;
    let code = input.take(code_length)?;

    let exceptions_len = input.parse_u16()? as usize;
    let exceptions = input.seq(exceptions_len, parse_exception_info)?;

    let attributes_len = input.parse_u16()? as usize;
    let attributes = input.seq(attributes_len, |input| parse_attribute(input, pool))?;

    Ok(Code {
        max_stack,
        max_locals,
        code: code.into(),
        exceptions: exceptions.into(),
        attributes: attributes.into(),
    })
}

// VerificationType {
//     tag: u8,
//     data: match tag {
//         0 => Top,
//         1 => Integer,
//         2 => Float,
//         3 => Double,
//         4 => Long,
//         5 => Null,
//         6 => UninitializedThis,
//         7 => Object {
//             index: u16,
//         },
//         8 => Uninitialized {
//             offset: u16,
//         },
//     }
// }
pub fn parse_verification_type(input: &mut ByteParser<'_>) -> ClassResult<VerificationType> {
    Ok(match input.parse_u8()? {
        0 => VerificationType::Top,
        1 => VerificationType::Integer,
        2 => VerificationType::Float,
        3 => VerificationType::Double,
        4 => VerificationType::Long,
        5 => VerificationType::Null,
        6 => VerificationType::UninitializedThis,
        7 => VerificationType::Object(input.parse_u16()? as usize - 1),
        8 => VerificationType::Uninitialized(input.parse_u16()? as usize),

        other => return Err(ClassError::UnknownVerificationType(other)),
    })
}

// StackMapFrame {
//     frame_type: u8,
//     frame: match frame_type {
//         0..=63 => StackMapFrame::Same {}
//         64..=127 => StackMapFrame::SameLocalsOneItem {
//             stack: VerificationType
//         }
//         247 => StackMapFrame::SameLocalsOneItemExtended {
//             offset_delta: u16
//             stack: VerificationType
//         }
//         248..=250 => StackMapFrame::Chop {
//             offset_delta: u16
//         }
//         251 => StackMapFrame::SameExtended {
//             offset_delta: u16
//         }
//         252..=254 => StackMapFrame::Append {
//             offset_delta: u16
//             locals: [VerificationType; frame_type - 251]
//         }
//         255 => StackMapFrame::Full {
//             offset_delta: u16
//             locals_count: u16
//             locals: [VerificationType; locals_count]
//             stack_count: u16
//             stack: [VerificationType; stack_count]
//         }
//     },
// }
pub fn parse_stack_map_frame(input: &mut ByteParser<'_>) -> ClassResult<StackMapFrame> {
    let frame_type = input.parse_u8()?;

    Ok(match frame_type {
        0..=63 => StackMapFrame::Same,
        64..=127 => StackMapFrame::SameLocalsOneItem(parse_verification_type(input)?),
        247 => {
            let delta = input.parse_u16()? as usize;
            let stack = parse_verification_type(input)?;
            StackMapFrame::SameLocalsOneItemExtended { delta, stack }
        }
        248..=250 => StackMapFrame::Chop(input.parse_u16()? as usize),
        251 => StackMapFrame::SameExtended(input.parse_u16()? as usize),
        252..=254 => {
            let delta = input.parse_u16()? as usize;
            let locals = input
                // but why?
                .seq(frame_type as usize - 251, parse_verification_type)?
                .into();
            StackMapFrame::Append { delta, locals }
        }
        255 => {
            let delta = input.parse_u16()? as usize;
            let locals_len = input.parse_u16()? as usize;
            let locals = input.seq(locals_len, parse_verification_type)?.into();
            let stack_len = input.parse_u16()? as usize;
            let stack = input.seq(stack_len, parse_verification_type)?.into();
            StackMapFrame::Full {
                delta,
                locals,
                stack,
            }
        }
        other => return Err(ClassError::UnknownStackMapFrameType(other)),
    })
}

pub fn parse_stack_map_table(input: &mut ByteParser<'_>) -> ClassResult<Box<[StackMapFrame]>> {
    let len = input.parse_u16()? as usize;
    input.seq(len, parse_stack_map_frame).map(Into::into)
}

pub(crate) fn parse_attribute(
    input: &mut ByteParser<'_>,
    pool: &[Constant],
) -> ClassResult<AttributeInfo> {
    let index = input.parse_u16()? as usize - 1;
    let len = input.parse_u32()? as usize;

    let attr = match &pool[index] {
        Constant::StringData(data) => Ok(match &**data {
            b"ConstantValue" => Attribute::ConstantValue(input.parse_u16()? as usize),
            b"Code" => Attribute::Code(parse_code(input, pool)?),
            b"StackMapTable" => Attribute::StackMapTable(parse_stack_map_table(input)?),
            _ => Attribute::Other(input.take(len)?.into()),
        }),
        _ => Err(ClassError::InvalidPoolType),
    }?;

    Ok(AttributeInfo { name: index, attr })
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AttributeInfo {
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the name of the attribute.
    pub name: PoolIndex,
    pub attr: Attribute,
}
