//! ```txt
//! Method {
//!     access_flags: u16,
//!     name_index: u16,
//!     descriptor_index: u16,
//!     attributes_count: u16,
//!     attributes: [AttributeInfo; attributes_count],
//! }
//! ```

use crate::{
    access::{FromAccessBitfield, MethodProperties},
    attribute::{parse_attribute, AttributeInfo},
    constant::{Constant, PoolIndex},
    field,
    parse::ByteParser,
    ClassError, ClassResult,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ReturnDescriptor {
    Void,
    Type(field::Descriptor),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Descriptor {
    pub ret: ReturnDescriptor,
    pub args: Box<[field::Descriptor]>,
}

pub fn parse_method_descriptor(input: &mut ByteParser<'_>) -> ClassResult<Descriptor> {
    input.expect(b"(")?;

    let mut args = Vec::new();
    loop {
        args.push(match input.peek(1)?[0] {
            b')' => break,
            _ => field::parse_field_descriptor(input)?,
        });
    }

    input.expect(b")")?;

    let ret = match input.peek(1)?[0] {
        b'V' => ReturnDescriptor::Void,
        _ => ReturnDescriptor::Type(field::parse_field_descriptor(input)?),
    };

    Ok(Descriptor {
        ret,
        args: args.into(),
    })
}

pub fn parse_method(input: &mut ByteParser<'_>, pool: &[Constant]) -> ClassResult<Method> {
    let flags = input.parse_u16()?;
    let properties =
        MethodProperties::from_bitfield(flags).ok_or(ClassError::InvalidAccessFlags(flags))?;
    let name = input.parse_u16()? as usize;
    let descriptor_index = input.parse_u16()? as usize;
    let descriptor = match pool[descriptor_index].as_string_data() {
        Some(data) => parse_method_descriptor(&mut ByteParser::new(data.as_bytes())),
        _ => Err(ClassError::InvalidPoolType),
    }?;

    let attributes_len = input.parse_u16()? as usize;
    let attributes = input.seq(attributes_len, |input| parse_attribute(input, pool))?;

    Ok(Method {
        properties,
        name,
        descriptor,
        attributes: attributes.into(),
    })
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Method {
    /// Method properties
    pub properties: MethodProperties,
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the name of the method.
    pub name: PoolIndex,
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the method signature.
    pub descriptor: Descriptor,
    pub attributes: Box<[AttributeInfo]>,
}
