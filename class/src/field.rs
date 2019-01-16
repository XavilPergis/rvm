//! ```txt
//! Field {
//!     access:           u16
//!     name:             u16
//!     descriptor:       u16
//!     attributes_count: u16
//!     attributes:       [Attribute; attributes_count]
//! }
//! ```

use crate::{
    access::{FieldProperties, FromAccessBitfield},
    attribute::{parse_attribute, AttributeInfo},
    constant::{Constant, PoolIndex},
    parse::{self, ByteParser},
    ClassError, ClassResult,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum BaseType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum FieldType {
    Primitive(BaseType),
    Object(String),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Descriptor {
    pub dimensions: usize,
    pub ty: FieldType,
}

pub fn parse_base_type(tag: u8) -> ClassResult<BaseType> {
    Ok(match tag {
        b'B' => BaseType::Byte,
        b'C' => BaseType::Char,
        b'D' => BaseType::Double,
        b'F' => BaseType::Float,
        b'I' => BaseType::Int,
        b'J' => BaseType::Long,
        b'S' => BaseType::Short,
        b'Z' => BaseType::Boolean,
        k => return Err(ClassError::InvalidBaseType(k)),
    })
}

pub fn parse_field_descriptor(input: &mut ByteParser<'_>) -> ClassResult<Descriptor> {
    match input.peek(1)?[0] {
        b'[' => {
            input.take(1)?;
            let descriptor = parse_field_descriptor(input)?;
            Ok(Descriptor {
                dimensions: descriptor.dimensions + 1,
                ..descriptor
            })
        }

        _ => Ok(Descriptor {
            dimensions: 0,
            ty: parse_field_descriptor_terminal(input)?,
        }),
    }
}

pub fn parse_field_descriptor_terminal(input: &mut ByteParser<'_>) -> ClassResult<FieldType> {
    // TODO: verify there's not extra gunk at the end of the descriptor
    Ok(match input.parse_u8()? {
        b'L' => FieldType::Object(parse::parse_mutf8(input.take_while(|ch| ch != b';')?)?.into()),

        other => match parse_base_type(other) {
            Ok(base) => FieldType::Primitive(base),
            Err(_) => return Err(ClassError::BadDescriptorType(other)),
        },
    })
}

pub fn parse_field(input: &mut ByteParser<'_>, pool: &[Constant]) -> ClassResult<Field> {
    let flags = input.parse_u16()?;
    let properties =
        FieldProperties::from_bitfield(flags).ok_or(ClassError::InvalidAccessFlags(flags))?;
    let name = input.parse_u16()? as usize;

    let descriptor_index = input.parse_u16()? as usize;
    let descriptor = match pool[descriptor_index].as_string_data() {
        Some(data) => parse_field_descriptor(&mut ByteParser::new(data.as_bytes())),
        _ => Err(ClassError::InvalidPoolType),
    }?;

    let attributes_len = input.parse_u16()? as usize;
    let attributes = input.seq(attributes_len, |input| parse_attribute(input, pool))?;

    Ok(Field {
        properties,
        name,
        descriptor,
        attributes: attributes.into(),
    })
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Field {
    pub properties: FieldProperties,
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the name of this field
    pub name: PoolIndex,
    /// The type for this field
    pub descriptor: Descriptor,

    pub attributes: Box<[AttributeInfo]>,
}
