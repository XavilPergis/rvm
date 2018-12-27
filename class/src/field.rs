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
    ByteParser, ClassError, ClassResult,
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

impl std::fmt::Display for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut was_written = false;
        let mut write = |s| {
            if was_written {
                write!(f, " {}", s)
            } else {
                write!(f, "{}", s)?;
                was_written = true;
                Ok(())
            }
        };

        if !self.is(Access::ENUM) {
            if self.is(Access::PUBLIC) {
                write("public")?;
            } else if self.is(Access::PROTECTED) {
                write("protected")?;
            } else if self.is(Access::PRIVATE) {
                write("private")?;
            }

            if self.is(Access::STATIC) {
                write("static")?;
            }

            if self.is(Access::FINAL) {
                write("final")?;
            } else if self.is(Access::TRANSIENT) {
                write("transient")?;
            }

            if self.is(Access::VOLATILE) {
                write("volatile")?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Descriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self.ty {
                FieldType::Byte => "byte",
                FieldType::Char => "char",
                FieldType::Double => "double",
                FieldType::Float => "float",
                FieldType::Int => "int",
                FieldType::Long => "long",
                FieldType::Short => "short",
                FieldType::Boolean => "boolean",
                FieldType::Object(name) => std::str::from_utf8(&name).unwrap_or("<not utf8>"),
            }
        )?;

        for _ in 0..self.dimensions {
            write!(f, "[]")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum FieldType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
    Object(Box<[u8]>),
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
        b'B' => FieldType::Byte,
        b'C' => FieldType::Char,
        b'D' => FieldType::Double,
        b'F' => FieldType::Float,
        b'I' => FieldType::Int,
        b'J' => FieldType::Long,
        b'S' => FieldType::Short,
        b'Z' => FieldType::Boolean,
        b'L' => FieldType::Object(
            input
                .take_while(|ch| ch != b';')?
                .split_last()
                .map(|(_, tail)| tail)
                .unwrap_or(b"")
                .into(),
        ),

        other => return Err(ClassError::BadDescriptorType(other)),
    })
}

pub(crate) fn parse_field(input: &mut ByteParser<'_>, pool: &[Constant]) -> ClassResult<Field> {
    let access = Access(input.parse_u16()?);
    let name = input.parse_u16()? as usize - 1;

    let descriptor_index = input.parse_u16()? as usize - 1;
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
