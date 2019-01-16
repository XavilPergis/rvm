//! # Attribute Formats
//!
//! ## General Form
//! The general form of an attribute is listed below. It is comprised of an
//! index into the constant pool, representing the name of the attribute,
//! followed by the beyte length of the *rest* of the attribute, not including
//! the first six bytes, followed by attribute-specific information, whose
//! length is decided by the attrinute type.
//!
//! ```txt
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
//! ```txt
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

use crate::{
    access::{FromAccessBitfield, InnerClassProperties},
    constant::{Constant, PoolIndex},
    field::BaseType,
    parse::{ByteParser, ParseError},
    ClassError, ClassResult,
};
use std::{collections::HashMap, ops::Range};

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

// TODO: move somewhere else; it doesn't belong here.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct HalfOpen<Idx> {
    pub start: Idx,
    pub end: Idx,
}

impl<I> From<Range<I>> for HalfOpen<I> {
    fn from(range: Range<I>) -> Self {
        HalfOpen {
            start: range.start,
            end: range.end,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExceptionInfo {
    pub active_region: HalfOpen<usize>,
    pub handler_pc: PoolIndex,
    pub catch_type: PoolIndex,
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct InnerClass {
    pub info: PoolIndex,
    pub outer_info: Option<PoolIndex>,
    pub name: Option<PoolIndex>,
    pub properties: InnerClassProperties,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct LocalVariable {
    /// The range of bytecode indices in which this local variable needs to have
    /// a value.
    pub range: HalfOpen<usize>,
    /// The name of this variable.
    pub name: PoolIndex,
    /// The constant pool index of either the field descriptor or the field
    /// signature of this variable.
    pub ty: PoolIndex,
    /// The Index into the local variable table that this local variable should
    /// appear at.
    pub lvt_index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Annotation {
    pub ty: PoolIndex,
    pub fields: Box<[AnnotationPair]>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AnnotationPair {
    pub name: PoolIndex,
    pub value: AnnotationValue,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum AnnotationValue {
    // Const value
    Primitive(BaseType, PoolIndex),
    String(PoolIndex),

    // Enum const value
    Enum { ty: PoolIndex, name: PoolIndex },

    Class(PoolIndex),
    Annotation(Box<Annotation>),
    Array(Box<[AnnotationValue]>),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BootstrapMethod {
    pub method: PoolIndex,
    pub arguments: Box<[PoolIndex]>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

    /// Contains either a class, method, or field signature, depending on what
    /// the attribute was declared on.
    Signature(PoolIndex),

    /// Represents which checked exceptions a method can throw. Each item is an
    /// index to a `Class` constant that represents a class that can be thrown.
    Exceptions(Box<[PoolIndex]>),

    /// Represents which classes this class contains. This attribute should only
    /// appear in a class attribute table.
    InnerClasses(Box<[InnerClass]>),

    /// Represents the enclosing method of a local or anonymous class. This
    /// attribute should only appear in a class attribute table.
    EnclosingMethod {
        class: PoolIndex,
        method: Option<PoolIndex>,
    },

    /// A marker that means this item does not appear anywhere in source code.
    /// If the item is synthetic, either this attribute must be present or the
    /// synthetic access flag must be set.
    Synthetic,

    /// Points to a `StringData` entry in the constant pool representing the
    /// file name of the class this is declared on. This attribute should only
    /// appear in a class attribute table.
    SourceFile(PoolIndex),

    /// Arbitrary extended debugging data. Shoould have no effect on the JVM.
    SourceDebugExtension(String),

    /// Represents a mapping between the start of an instruction (offset into
    /// the `Code` attribute) and the line number this instruction was generated
    /// from.
    LineNumberTable(HashMap<usize, usize>),

    /// Type-erased local variable table.
    LocalVariableTable(Box<[LocalVariable]>),
    /// Fully elaborated local variable table.
    LocalVariableTypeTable(Box<[LocalVariable]>),

    /// A marker that means this item is deprecated and should not be used in
    /// the future. This should not have any effect on semantics.
    Deprecated,

    /// Represents annotations that are available for reflective APIs to
    /// consume. At most one of this attribute may appear on a class, field, or
    /// method.
    RuntimeVisibleAnnotations(Box<[Annotation]>),
    /// Represents annotations that are **not** available for reflective APIs to
    /// consume. At most one of this attribute may appear on a class, field, or
    /// method.
    RuntimeInvisibleAnnotations(Box<[Annotation]>),
    RuntimeVisibleParameterAnnotations(Box<[Box<[Annotation]>]>),
    RuntimeInvisibleParameterAnnotations(Box<[Box<[Annotation]>]>),

    AnnotationDefault(AnnotationValue),
    BootstrapMethods(Box<[BootstrapMethod]>),
}

fn parse_pool_index(input: &mut ByteParser<'_>) -> ClassResult<Option<usize>> {
    input
        .parse_u16()
        .map(|idx| match idx {
            0 => None,
            k => Some(k as usize),
        })
        .map_err(Into::into)
}

fn parse_pool_index_nonzero(input: &mut ByteParser<'_>) -> ClassResult<usize> {
    parse_pool_index(input).and_then(|idx| idx.ok_or(ClassError::InvalidPoolIndex))
}

pub fn parse_string(input: &mut ByteParser<'_>, len: usize) -> ClassResult<String> {
    crate::mutf8::parse_mutf8(input.take(len)?).map(Into::into)
}

// AnnotationValue {
//     tag: u8,
//     value: match tag {
//         b'B' | b'C' | b'D' | b'F' | b'I' | b'J' | b'S' | b'Z' =>
//             AnnotationValue::Primitive(BaseType, u16),
//         b's' => AnnotationValue::String(u16),
//         b'e' => AnnotationValue::EnumConst(u16, u16),
//         b'c' => AnnotationValue::Class(u16),
//         b'@' => AnnotationValue::Annotation(Annotation),
//         b'[' => AnnotationValue::Array {
//             num_values: u16,
//             values: [AnnotationValue; num_values],
//         },
//     },
// }
pub fn parse_annotation_value(input: &mut ByteParser<'_>) -> ClassResult<AnnotationValue> {
    Ok(match input.parse_u8()? {
        b's' => AnnotationValue::String(input.parse_u16()? as usize),
        b'e' => AnnotationValue::Enum {
            ty: input.parse_u16()? as usize,
            name: input.parse_u16()? as usize,
        },
        b'c' => AnnotationValue::Class(input.parse_u16()? as usize),
        b'@' => AnnotationValue::Annotation(Box::new(parse_annotation(input)?)),
        b'[' => AnnotationValue::Array({
            let len = input.parse_u16()? as usize;
            input.seq(len, parse_annotation_value)?.into()
        }),

        tag => AnnotationValue::Primitive(
            crate::field::parse_base_type(tag)?,
            input.parse_u16()? as usize,
        ),
    })
}

// BootstrapMethod {
//     bootstrap_method_ref: u16,
//     num_bootstrap_arguments: u16,
//     bootstrap_arguments: [u16; num_bootstrap_arguments],
// }
pub fn parse_bootstrap_method(input: &mut ByteParser<'_>) -> ClassResult<BootstrapMethod> {
    Ok(BootstrapMethod {
        method: parse_pool_index_nonzero(input)?,
        arguments: {
            let len = input.parse_u16()? as usize;
            input.seq(len, parse_pool_index_nonzero)?.into()
        },
    })
}

// Attribute::BootstrapMethods {
//     num_bootstrap_methods: u16,
//     bootstrap_methods: [BootstrapMethod; num_bootstrap_methods],
// }
pub fn parse_bootstrap_methods(input: &mut ByteParser<'_>) -> ClassResult<Box<[BootstrapMethod]>> {
    let num = input.parse_u16()? as usize;
    input.seq(num, parse_bootstrap_method).map(Into::into)
}

// AnnotationPair {
//     element_name_index: u16,
//     value: AnnotationValue,
// }
pub fn parse_annotation_pair(input: &mut ByteParser<'_>) -> ClassResult<AnnotationPair> {
    Ok(AnnotationPair {
        name: input.parse_u16()? as usize,
        value: parse_annotation_value(input)?,
    })
}

// Annotation {
//     type_index: u16,
//     num_element_value_pairs: u16,
//     element_value_pairs: [AnnotationPair; num_element_value_pairs],
// }
pub fn parse_annotation(input: &mut ByteParser<'_>) -> ClassResult<Annotation> {
    Ok(Annotation {
        ty: input.parse_u16()? as usize,
        fields: {
            let len = input.parse_u16()? as usize;
            input.seq(len, parse_annotation_pair)?.into()
        },
    })
}

// Annotation::RuntimeVisibleAnnotations,
// Annotation::RuntimeInvisibleAnnotations {
//     annotations: Annotations,
// }
//
// Annotations {
//     num_annotations: u16,
//     annotations: [Annotation; num_annotations],
// }
pub fn parse_annotations(input: &mut ByteParser<'_>) -> ClassResult<Box<[Annotation]>> {
    let num = input.parse_u16()? as usize;
    input.seq(num, parse_annotation).map(Into::into)
}

// Attribute::RuntimeVisibleParameterAnnotations,
// Attribute::RuntimeInvisibleParameterAnnotations {
//     num_parameters: u8,
//     parameter_annotations: [Annotations; num_parameters],
// }
pub fn parse_parameter_annotations(
    input: &mut ByteParser<'_>,
) -> ClassResult<Box<[Box<[Annotation]>]>> {
    let num = input.parse_u8()? as usize;
    input.seq(num, parse_annotations).map(Into::into)
}

// LocalVariable {
//     start_pc: u16,
//     length: u16,
//     name_index: u16,
//     descriptor_index: u16,
//     index: u16,
// }
pub fn parse_local_variable(input: &mut ByteParser<'_>) -> ClassResult<LocalVariable> {
    let start = input.parse_u16()? as usize;
    let end = start + input.parse_u16()? as usize;

    Ok(LocalVariable {
        range: HalfOpen::from(start..end),
        name: input.parse_u16()? as usize,
        ty: input.parse_u16()? as usize,
        lvt_index: input.parse_u16()? as usize,
    })
}

// Attribute::LocalVariableTable {
//     local_variable_table_len: u16,
//     local_variable_table: [LocalVariable; local_variable_table_len],
// }
pub fn parse_local_variable_table(input: &mut ByteParser<'_>) -> ClassResult<Box<[LocalVariable]>> {
    let num = input.parse_u16()? as usize;
    input.seq(num, parse_local_variable).map(Into::into)
}

// LineNumberMapping {
//     start_pc: u16,
//     line_number: u16,
// }
//
// Attribute::LineNumberTable {
//     line_number_table_len: u16,
//     line_number_table: [LineNumberMapping; line_number_table_len];
// }
pub fn parse_line_numer_table(input: &mut ByteParser<'_>) -> ClassResult<HashMap<usize, usize>> {
    let len = input.parse_u16()? as usize;
    Ok((0..len)
        .map(|_| Ok((input.parse_u16()? as usize, input.parse_u16()? as usize)))
        .collect::<Result<_, ParseError>>()?)
}

// InnerClass {
//     inner_class_info_index: u16,
//     outer_class_info_index: u16,
//     inner_name_index: u16,
//     inner_class_access_flags: u16,
// }
pub fn parse_inner_class(input: &mut ByteParser<'_>) -> ClassResult<InnerClass> {
    Ok(InnerClass {
        info: parse_pool_index_nonzero(input)?,
        outer_info: parse_pool_index(input)?,
        name: parse_pool_index(input)?,
        properties: {
            let idx = input.parse_u16()?;
            InnerClassProperties::from_bitfield(idx).ok_or(ClassError::InvalidAccessFlags(idx))?
        },
    })
}

// Attribute::InnerClasses {
//     classes_len: u16,
//     classes: [InnerClass; classes_len],
// }
pub fn parse_inner_classes(input: &mut ByteParser<'_>) -> ClassResult<Box<[InnerClass]>> {
    let num = input.parse_u16()? as usize;
    input.seq(num, parse_inner_class).map(Into::into)
}

// Attribute::Exceptions {
//     exceptions_len: u16,
//     exception_index_table: [u16; exceptions_len],
// }
pub fn parse_exceptions(input: &mut ByteParser<'_>) -> ClassResult<Box<[PoolIndex]>> {
    let len = input.parse_u16()? as usize;
    Ok(input
        .seq(len, |input| input.parse_u16().map(|idx| idx as usize))?
        .into())
}

// ExceptionInfo {
//     start_pc: u16,
//     end_pc: u16,
//     handler_pc: u16,
//     catch_type: u16,
// }
pub fn parse_exception_info(input: &mut ByteParser<'_>) -> ClassResult<ExceptionInfo> {
    let start = input.parse_u16()? as usize;
    let end = input.parse_u16()? as usize;
    let handler_pc = input.parse_u16()? as usize;
    let catch_type = input.parse_u16()? as usize;

    Ok(ExceptionInfo {
        active_region: HalfOpen::from(start..end),
        handler_pc,
        catch_type,
    })
}

// Attribute::Code {
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
pub fn parse_code(input: &mut ByteParser<'_>, pool: &[Constant]) -> ClassResult<Code> {
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
        7 => VerificationType::Object(input.parse_u16()? as usize),
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

pub fn parse_attribute(
    input: &mut ByteParser<'_>,
    pool: &[Constant],
) -> ClassResult<AttributeInfo> {
    let index = input.parse_u16()? as usize;
    let len = input.parse_u32()? as usize;

    let attr = match &pool[index] {
        Constant::StringData(data) => Ok(match &**data {
            "ConstantValue" => Attribute::ConstantValue(input.parse_u16()? as usize),
            "Code" => Attribute::Code(parse_code(input, pool)?),
            "StackMapTable" => Attribute::StackMapTable(parse_stack_map_table(input)?),
            "Exceptions" => Attribute::Exceptions(parse_exceptions(input)?),
            "InnerClasses" => Attribute::InnerClasses(parse_inner_classes(input)?),
            "EnclosingMethod" => Attribute::EnclosingMethod {
                class: parse_pool_index_nonzero(input)?,
                method: parse_pool_index(input)?,
            },
            "Synthetic" => Attribute::Synthetic,
            "Signature" => Attribute::Signature(input.parse_u16()? as usize),
            "SourceFile" => Attribute::SourceFile(parse_pool_index_nonzero(input)?),
            "SourceDebugExtension" => Attribute::SourceDebugExtension(parse_string(input, len)?),
            "LineNumberTable" => Attribute::LineNumberTable(parse_line_numer_table(input)?),
            "LocalVariableTable" => {
                Attribute::LocalVariableTable(parse_local_variable_table(input)?)
            }
            "LocalVariableTypeTable" => {
                Attribute::LocalVariableTypeTable(parse_local_variable_table(input)?)
            }
            "Deprecated" => Attribute::Deprecated,

            "RuntimeVisibleAnnotations" => {
                Attribute::RuntimeVisibleAnnotations(parse_annotations(input)?)
            }
            "RuntimeInvisibleAnnotations" => {
                Attribute::RuntimeInvisibleAnnotations(parse_annotations(input)?)
            }
            "RuntimeVisibleParameterAnnotations" => {
                Attribute::RuntimeVisibleParameterAnnotations(parse_parameter_annotations(input)?)
            }
            "RuntimeInvisibleParameterAnnotations" => {
                Attribute::RuntimeInvisibleParameterAnnotations(parse_parameter_annotations(input)?)
            }
            "AnnotationDefault" => Attribute::AnnotationDefault(parse_annotation_value(input)?),
            "BootstrapMethods" => Attribute::BootstrapMethods(parse_bootstrap_methods(input)?),
            _ => Attribute::Other(input.take(len)?.into()),
        }),
        _ => Err(ClassError::InvalidPoolType),
    }?;

    Ok(AttributeInfo { name: index, attr })
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AttributeInfo {
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the name of the attribute.
    pub name: PoolIndex,
    pub attr: Attribute,
}
