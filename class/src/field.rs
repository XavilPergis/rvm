//! ```
//! Field {
//!     access:           u16
//!     name:             u16
//!     descriptor:       u16
//!     attributes_count: u16
//!     attributes:       [Attribute; attributes_count]
//! }
//! ```

use crate::{
    attribute::{parse_attribute, AttributeInfo},
    constant::{Constant, PoolIndex},
    ByteParser, ClassError, ClassResult, Jtf,
};

/// Properties and access patterns of this field.
///
/// If this field is part of an
/// interface, then the `public`, `final`, and `static` flags must all be set,
/// and no other flags except `synthetic` can be set.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Access(u16);

impl Access {
    /// Declared as an element of an enum.
    pub const ENUM: Access = Access(0x4000);
    /// Declared final; it is never directly assigned to after
    /// object construction. If this flag is set, the `volatile` flag must not
    /// be set.
    pub const FINAL: Access = Access(0x0010);
    /// Declared private; usable only within the defining class. If this flag is
    /// set, no other visibility flag can be set.
    pub const PRIVATE: Access = Access(0x0002);
    /// Declared protected; may be accessed within subclasses. If this flag is
    /// set, no other visibility flag can be set.
    pub const PROTECTED: Access = Access(0x0004);
    /// Declared public; may be accessed from outside its package. If this flag
    /// is set, no other visibility flag can be set.
    pub const PUBLIC: Access = Access(0x0001);
    /// Declared static.
    pub const STATIC: Access = Access(0x0008);
    /// Declared synthetic; not present in the source code.
    pub const SYNTHETIC: Access = Access(0x1000);
    /// Declared transient; not written or read by a persistent object manager.
    pub const TRANSIENT: Access = Access(0x0080);
    /// Declared volatile; cannot be cached. If this flag is set, the `final`
    /// flag must not be set.
    pub const VOLATILE: Access = Access(0x0040);

    pub fn is(self, access: Access) -> bool {
        self & access != Access(0)
    }

    pub fn into_raw(self) -> u16 {
        self.0
    }
}

impl std::ops::BitAnd for Access {
    type Output = Access;

    fn bitand(self, other: Access) -> Access {
        Access(self.0 & other.0)
    }
}

impl std::ops::BitOr for Access {
    type Output = Access;

    fn bitor(self, other: Access) -> Access {
        Access(self.0 | other.0)
    }
}

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
    Object(Jtf),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Descriptor {
    pub dimensions: usize,
    pub ty: FieldType,
}

pub(crate) fn parse_field_descriptor(input: &mut ByteParser<'_>) -> ClassResult<Descriptor> {
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

pub(crate) fn parse_field_descriptor_terminal(
    input: &mut ByteParser<'_>,
) -> ClassResult<FieldType> {
    // TODO: verify there's not extra gunk at the end of the descriptor
    Ok(match input.parse_u8()? {
        b'B' => FieldType::Primitive(BaseType::Byte),
        b'C' => FieldType::Primitive(BaseType::Char),
        b'D' => FieldType::Primitive(BaseType::Double),
        b'F' => FieldType::Primitive(BaseType::Float),
        b'I' => FieldType::Primitive(BaseType::Int),
        b'J' => FieldType::Primitive(BaseType::Long),
        b'S' => FieldType::Primitive(BaseType::Short),
        b'Z' => FieldType::Primitive(BaseType::Boolean),
        b'L' => FieldType::Object(Jtf(input
            .take_while(|ch| ch != b';')?
            .split_last()
            .map(|(_, tail)| tail)
            .unwrap_or(b"")
            .into())),

        other => return Err(ClassError::BadDescriptorType(other)),
    })
}

pub(crate) fn parse_field(input: &mut ByteParser<'_>, pool: &[Constant]) -> ClassResult<Field> {
    let access = Access(input.parse_u16()?);
    let name = input.parse_u16()? as usize;

    let descriptor_index = input.parse_u16()? as usize;
    let descriptor = match pool[descriptor_index].as_string_data() {
        Some(data) => parse_field_descriptor(&mut ByteParser::new(data)),
        _ => Err(ClassError::InvalidPoolType),
    }?;

    let attributes_len = input.parse_u16()? as usize;
    let attributes = input.seq(attributes_len, |input| parse_attribute(input, pool))?;

    Ok(Field {
        access,
        name,
        descriptor,
        attributes: attributes.into(),
    })
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Field {
    pub access: Access,
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the name of this field
    pub name: PoolIndex,
    /// The type for this field
    pub descriptor: Descriptor,

    pub attributes: Box<[AttributeInfo]>,
}
