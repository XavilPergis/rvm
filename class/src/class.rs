//! # Class file binary format
//!
//! All numbers in this format are big-endian
//!
//! ```txt
//! Class {
//!     // 0xCAFEBABE
//!     magic: u32,
//!     minor_version: u16,
//!     major_version: u16,
//!
//!     constant_pool_count: u16,
//!     constant_pool: [Constant; constant_pool_count - 1],
//!
//!     access_flags: u16,
//!     this_class: u16,
//!     super_class: u16,
//!
//!     interfaces_count: u16,
//!     interfaces: [u16; interfaces_count],
//!
//!     fields_count: u16,
//!     fields: [Field; fields_count],
//!
//!     methods_count: u16,
//!     methods: [Method; methods_count],
//!
//!     attributes_count: u16,
//!     attributes: [Attribute; attributes_count],
//! }
//! ```

use crate::{
    access::{ClassProperties, FromAccessBitfield},
    attribute::{parse_attribute, AttributeInfo},
    constant::{parse_constant_pool, Constant},
    field::{parse_field, Field},
    method::{parse_method, Method},
    parse::{ByteParser, ParseResult},
    ClassError, ClassResult,
};

pub fn parse_version(input: &mut ByteParser<'_>) -> ParseResult<Version> {
    let minor = input.parse_u16()?;
    let major = input.parse_u16()?;
    Ok(Version { minor, major })
}

pub fn parse_access_flags(input: &mut ByteParser<'_>) -> ClassResult<ClassProperties> {
    let flags = input.parse_u16()?;
    ClassProperties::from_bitfield(flags).ok_or(ClassError::InvalidAccessFlags(flags))
}

/// The class file magic: `0xCAFEBABE`
pub const CLASS_MAGIC: &[u8; 4] = &[0xCA, 0xFE, 0xBA, 0xBE];

pub fn parse_class(input: &mut ByteParser<'_>) -> ClassResult<Class> {
    input.expect(CLASS_MAGIC)?;
    let version = parse_version(input)?;
    let pool = parse_constant_pool(input)?;
    let properties = parse_access_flags(input)?;
    let this_class = input.parse_u16()? as usize;
    let super_class = input.parse_u16()? as usize;

    let interfaces_len = input.parse_u16()? as usize;
    let interfaces = input.seq(interfaces_len, |input| {
        input.parse_u16().map(|x| x as usize)
    })?;

    let fields_len = input.parse_u16()? as usize;
    let fields = input.seq(fields_len, |input| parse_field(input, &pool))?;

    let methods_len = input.parse_u16()? as usize;
    let methods = input.seq(methods_len, |input| parse_method(input, &pool))?;

    let attributes_len = input.parse_u16()? as usize;
    let attributes = input.seq(attributes_len, |input| parse_attribute(input, &pool))?;

    Ok(Class {
        version,
        pool,
        properties,
        this_class,
        super_class,
        interfaces: interfaces.into(),
        fields: fields.into(),
        methods: methods.into(),
        attributes: attributes.into(),
    })
}

/// Version of the class file.
///
/// Versions are denoted as `M.m` where `M` is the
/// major version and `m` is the minor version. The version can be ordered
/// lexicographically. JVM implementations can choose a range of compatible
/// versions by selecting a minimum major version `Mi`, maximum major version
/// `Mj`, and a maximum minor version `m`. With these, the JVM can support
/// versions on the range `Mi.0 <= v <= Mj.m` for some arbitrary `v`.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Version {
    pub major: u16,
    pub minor: u16,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub version: Version,
    pub properties: ClassProperties,

    pub this_class: usize,
    pub super_class: usize,
    pub pool: Box<[Constant]>,

    pub interfaces: Box<[usize]>,
    pub fields: Box<[Field]>,
    pub methods: Box<[Method]>,
    pub attributes: Box<[AttributeInfo]>,
}

impl Class {
    pub fn parse<T: AsRef<[u8]>>(src: T) -> Result<Class, ClassError> {
        parse_class(&mut ByteParser::new(src.as_ref()))
    }
}
