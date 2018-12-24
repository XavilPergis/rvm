//! # Class file binary format
//!
//! All numbers in this format are big-endian
//!
//! ```
//! Class {
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
//!
//! ## Constant Pool
//! Entries in the constant pool start at 1, and indices into the pool are
//! likewise 1-based. Each entry is comprised of a 1-byte tag, followed by a
//! variable length of bytes decided by the type of constant.
//!
//! ### Oddities
//! Utf8 constants aren't actually UTF-8, but a slightly modified UTF-8 as
//! described in §4.4.7
//!
//! Long and Double entries take up two slots in the constant pool, but the
//! upper entry is never directly referenced.
//! ```
//! // tag = 1
//! Constant::Utf8 {
//!     tag:    u8
//!     length: u16
//!     data:   [u8; length]
//! }
//!
//! // tag = 3
//! Constant::Int {
//!     tag:  u8
//!     data: i32
//! }
//!
//! // tag = 4
//! Constant::Float {
//!     tag:  u8
//!     data: f32
//! }
//!
//! // tag = 5
//! Constant::Long {
//!     tag:  u8
//!     data: i64
//! }
//!
//! // tag = 6
//! Constant::Double {
//!     tag:  u8
//!     data: f64
//! }
//!
//! Constant::Class // tag = 7
//! Constant::String // tag = 8
//! Constant::Method Type // tag = 16
//! {
//!     tag:   u8
//!     index: u16 // index of string data
//! }
//!
//! Constant::Field Ref // tag = 9
//! Constant::Method Ref // tag = 10
//! Constant::Interface Method Ref // tag = 11
//! {
//!     tag:       u8
//!     class:     u16 // index of class
//!     name_type: u16 // index of name and type
//! }
//!
//! // tag = 12
//! Constant::Name And Type {
//!     tag:       u8
//!     class:     u16 // index of class
//!     name_type: u16 // index of name and type
//! }
//!
//! // tag = 15
//! Constant::Method Handle {
//!     tag:   u8
//!     kind:  u8  // index of class
//!     index: u16 // index of whatever `kind` requires
//! }
//!
//! // tag = 18
//! Constant::Invoke Dynamic {
//!     tag:       u8
//!     bootstrap: u8  // 0-based index into the bootstrap method table
//!     name_type: u16 // index of name and type
//! }
//! ```

use crate::raw::{
    attribute::{parse_attribute, AttributeError, AttributeInfo},
    constant::{parse_constant_pool, Constant, ConstantError},
    field::{parse_field, Field},
    method::{parse_method, Method},
    ByteParser, ParseError, ParseResult,
};

fn parse_version(input: &mut ByteParser<'_>) -> ParseResult<Version> {
    let minor = input.parse_u16()?;
    let major = input.parse_u16()?;
    Ok(Version { minor, major })
}

fn parse_access_flags(input: &mut ByteParser<'_>) -> ParseResult<Access> {
    Ok(Access(input.parse_u16()?))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ClassError {
    WrongMagic,
    Constant(ConstantError),
    Parse(ParseError),
    Attribute(AttributeError),
}

impl From<ParseError> for ClassError {
    fn from(err: ParseError) -> ClassError {
        ClassError::Parse(err)
    }
}

impl From<ConstantError> for ClassError {
    fn from(err: ConstantError) -> ClassError {
        ClassError::Constant(err)
    }
}

impl From<AttributeError> for ClassError {
    fn from(err: AttributeError) -> ClassError {
        ClassError::Attribute(err)
    }
}

/// The class file magic: `0xCAFEBABE`
pub const CLASS_MAGIC: &[u8; 4] = &[0xCA, 0xFE, 0xBA, 0xBE];

fn parse_class(input: &mut ByteParser<'_>) -> Result<Class, ClassError> {
    input.tag(CLASS_MAGIC)?;
    let version = parse_version(input)?;
    let constant_pool = parse_constant_pool(input)?;
    let access_flags = parse_access_flags(input)?;
    let this_class = input.parse_u16()? as usize - 1;
    let super_class = input.parse_u16()? as usize - 1;

    let interfaces_len = input.parse_u16()? as usize;
    let interfaces = input.seq(interfaces_len, |input| {
        input.parse_u16().map(|x| x as usize)
    })?;

    let fields_len = input.parse_u16()? as usize;
    let fields = input.seq(fields_len, |input| parse_field(input, &constant_pool))?;

    let methods_len = input.parse_u16()? as usize;
    let methods = input.seq(methods_len, |input| parse_method(input, &constant_pool))?;

    let attributes_len = input.parse_u16()? as usize;
    let attributes = input.seq(attributes_len, |input| {
        parse_attribute(input, &constant_pool)
    })?;

    Ok(Class {
        version,
        constant_pool,
        access_flags,
        this_class,
        super_class,
        interfaces: interfaces.into(),
        fields: fields.into(),
        methods: methods.into(),
        attributes: attributes.into(),
    })
}

/// Access flags denote the properties of a given class file.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Access(u16);

impl Access {
    /// The class is declared `abstract`; it cannot be directly instantiated. If
    /// this flag is set, then the `final` flag must not be set.
    pub const ABSTRACT: Access = Access(0x0400);
    /// The class is an annotation. If this flag is set, the `interface` flag
    /// must also be set.
    pub const ANNOTATION: Access = Access(0x2000);
    /// The class or a supertype of the class is an `enum`.
    pub const ENUM: Access = Access(0x4000);
    /// The class is desclared as `final`; no subclasses are allowed. If this
    /// flag is set, then the `abstract` flag must not be set.
    pub const FINAL: Access = Access(0x0010);
    /// The class is an `interface`. If this flag is set, the abstract flag must
    /// also be set. Additionally, the `final`, `super`, and `enum` flags must
    /// not be
    pub const INTERFACE: Access = Access(0x0200);
    /// The class is declared as `public`; it may be accessed from outside the
    /// current package.
    pub const PUBLIC: Access = Access(0x0001);
    /// Treat superclass methods specially when invoked by the invokespecial
    /// instruction.
    pub const SUPER: Access = Access(0x0020);
    /// The class does not appear in source code; it was generated by the
    /// compiler.
    pub const SYNTHETIC: Access = Access(0x1000);

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

impl std::fmt::Debug for Access {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({:X}) ", self.0)?;
        if self.is(Access::SUPER) {
            write!(f, "[super] ")?;
        }
        if self.is(Access::SYNTHETIC) {
            write!(f, "[synthetic] ")?;
        }

        if self.is(Access::PUBLIC) {
            write!(f, "public ")?;
        }
        if self.is(Access::FINAL) {
            write!(f, "final ")?;
        }
        if self.is(Access::ABSTRACT) && !self.is(Access::INTERFACE) {
            write!(f, "abstract ")?;
        }

        if self.is(Access::ANNOTATION) {
            write!(f, "annotation ")?;
        } else if self.is(Access::INTERFACE) {
            write!(f, "interface ")?;
        } else if self.is(Access::ENUM) {
            write!(f, "enum ")?;
        } else {
            write!(f, "class ")?;
        }

        Ok(())
    }
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
    pub access_flags: Access,

    pub this_class: usize,
    pub super_class: usize,
    pub constant_pool: Box<[Constant]>,

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
