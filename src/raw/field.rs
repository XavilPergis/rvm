//! ```
//! Field {
//!     access:           u16
//!     name:             u16
//!     descriptor:       u16
//!     attributes_count: u16
//!     attributes:       [Attribute; attributes_count]
//! }
//! ```

use crate::raw::{
    attribute::{parse_attribute, AttributeInfo, AttributeResult},
    constant::{Constant, PoolIndex},
    ByteParser, *,
};

/// Properties and access patterns of this field. If this field is part of an
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
}

pub(crate) fn parse_field(input: &mut ByteParser<'_>, pool: &[Constant]) -> AttributeResult<Field> {
    let access = Access(input.parse_u16()?);
    let name = input.parse_u16()? as usize - 1;
    let descriptor = input.parse_u16()? as usize - 1;
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
    /// Index into the constant pool, pointing to a `Constant::StringData` that
    /// denotes the type of this field
    pub descriptor: PoolIndex,

    pub attributes: Box<[AttributeInfo]>,
}
