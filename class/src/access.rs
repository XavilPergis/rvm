pub struct AccessFlags(pub u16);

use std::ops::*;

impl BitOr for AccessFlags {
    type Output = AccessFlags;

    fn bitor(self, other: Self) -> Self {
        AccessFlags(self.0 | other.0)
    }
}

impl BitAnd for AccessFlags {
    type Output = AccessFlags;

    fn bitand(self, other: Self) -> Self {
        AccessFlags(self.0 & other.0)
    }
}

impl BitXor for AccessFlags {
    type Output = AccessFlags;

    fn bitxor(self, other: Self) -> Self {
        AccessFlags(self.0 ^ other.0)
    }
}

pub trait ToAccessBitfield {
    fn to_bitfield(self) -> AccessFlags;
}

pub trait FromAccessBitfield: Sized {
    fn verify(flags: u16) -> bool;
    fn from_bitfield_unchecked(flags: u16) -> Self;

    fn from_bitfield(flags: u16) -> Option<Self> {
        if Self::verify(flags) {
            Some(Self::from_bitfield_unchecked(flags))
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Access {
    Public,
    Protected,
    PackagePrivate,
    Private,
}

impl ToAccessBitfield for Access {
    fn to_bitfield(self) -> AccessFlags {
        AccessFlags(match self {
            Access::Public => ACC_PUBLIC,
            Access::Protected => ACC_PROTECTED,
            Access::PackagePrivate => 0,
            Access::Private => ACC_PRIVATE,
        })
    }
}

impl FromAccessBitfield for Access {
    fn verify(flags: u16) -> bool {
        let mask = ACC_PUBLIC | ACC_PROTECTED | ACC_PRIVATE;
        (flags & mask).count_ones() <= 1
    }

    fn from_bitfield_unchecked(flags: u16) -> Self {
        if flags & ACC_PUBLIC != 0 {
            Access::Public
        } else if flags & ACC_PROTECTED != 0 {
            Access::Protected
        } else if flags & ACC_PRIVATE != 0 {
            Access::Private
        } else {
            Access::PackagePrivate
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ClassType {
    Class,
    Enum,
    Interface,
    Annotation,
}

impl ToAccessBitfield for ClassType {
    fn to_bitfield(self) -> AccessFlags {
        AccessFlags(match self {
            ClassType::Class => 0,
            ClassType::Enum => ACC_ENUM,
            ClassType::Interface => ACC_INTERFACE,
            ClassType::Annotation => ACC_ANNOTATION,
        })
    }
}

impl FromAccessBitfield for ClassType {
    fn verify(flags: u16) -> bool {
        if flags & ACC_ENUM != 0 && flags & (ACC_INTERFACE | ACC_ANNOTATION) != 0 {
            return false;
        }

        if flags & ACC_ANNOTATION != 0 && flags & ACC_INTERFACE == 0 {
            return false;
        }

        true
    }

    fn from_bitfield_unchecked(flags: u16) -> Self {
        if flags & ACC_ENUM != 0 {
            ClassType::Enum
        } else if flags & ACC_ANNOTATION != 0 {
            ClassType::Annotation
        } else if flags & ACC_INTERFACE != 0 {
            ClassType::Interface
        } else {
            ClassType::Class
        }
    }
}

fn flag(flag: u16, val: bool) -> AccessFlags {
    AccessFlags(if val { flag } else { 0 })
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ClassProperties {
    pub access: Access,
    pub ty: ClassType,
    pub is_final: bool,
    pub is_abstract: bool,
    pub treat_super_specially: bool,
    pub is_compiler_generated: bool,
}

impl ToAccessBitfield for ClassProperties {
    fn to_bitfield(self) -> AccessFlags {
        self.access.to_bitfield()
            | self.ty.to_bitfield()
            | flag(ACC_SYNTHETIC, self.is_compiler_generated)
            | flag(ACC_FINAL, self.is_final)
            | flag(ACC_ABSTRACT, self.is_abstract)
            | flag(ACC_SUPER, self.treat_super_specially)
    }
}

impl FromAccessBitfield for ClassProperties {
    fn verify(flags: u16) -> bool {
        if !Access::verify(flags) | !ClassType::verify(flags) {
            return false;
        }

        // If annotation is set, then interface must also be set
        if flags & ACC_ANNOTATION != 0 && flags & ACC_INTERFACE == 0 {
            return false;
        }

        // If interface is set, abstract must also be set, and none of final, super, or
        // enum may be set.
        if flags & ACC_INTERFACE != 0
            && (flags & ACC_ABSTRACT == 0 || flags & (ACC_FINAL | ACC_SUPER | ACC_ENUM) != 0)
        {
            return false;
        }

        // Cannot be abstract and final.
        if flags & ACC_FINAL != 0 && flags & ACC_ABSTRACT != 0 {
            return false;
        }

        true
    }

    fn from_bitfield_unchecked(flags: u16) -> Self {
        ClassProperties {
            access: Access::from_bitfield_unchecked(flags),
            ty: ClassType::from_bitfield_unchecked(flags),
            is_final: flags & ACC_FINAL != 0,
            is_abstract: flags & ACC_ABSTRACT != 0,
            treat_super_specially: flags & ACC_SUPER != 0,
            is_compiler_generated: flags & ACC_SYNTHETIC != 0,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct InnerClassProperties {
    pub access: Access,
    pub ty: ClassType,
    pub is_final: bool,
    pub is_static: bool,
    pub is_abstract: bool,
    pub is_compiler_generated: bool,
}

impl ToAccessBitfield for InnerClassProperties {
    fn to_bitfield(self) -> AccessFlags {
        self.access.to_bitfield()
            | self.ty.to_bitfield()
            | flag(ACC_SYNTHETIC, self.is_compiler_generated)
            | flag(ACC_STATIC, self.is_static)
            | flag(ACC_FINAL, self.is_final)
            | flag(ACC_ABSTRACT, self.is_abstract)
    }
}

/// FIXME: I'm just guessing about the requirements here... I didn't see them in
/// the spec.
impl FromAccessBitfield for InnerClassProperties {
    fn verify(flags: u16) -> bool {
        ClassProperties::verify(flags)
    }

    fn from_bitfield_unchecked(flags: u16) -> Self {
        InnerClassProperties {
            access: Access::from_bitfield_unchecked(flags),
            ty: ClassType::from_bitfield_unchecked(flags),
            is_final: flags & ACC_FINAL != 0,
            is_abstract: flags & ACC_ABSTRACT != 0,
            is_static: flags & ACC_STATIC != 0,
            is_compiler_generated: flags & ACC_SYNTHETIC != 0,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct FieldProperties {
    pub access: Access,
    pub is_final: bool,
    pub is_static: bool,
    pub is_volatile: bool,
    pub is_transient: bool,
    pub is_enum: bool,
    pub is_compiler_generated: bool,
}

impl ToAccessBitfield for FieldProperties {
    fn to_bitfield(self) -> AccessFlags {
        self.access.to_bitfield()
            | flag(ACC_FINAL, self.is_final)
            | flag(ACC_STATIC, self.is_static)
            | flag(ACC_VOLATILE, self.is_volatile)
            | flag(ACC_TRANSIENT, self.is_transient)
            | flag(ACC_ENUM, self.is_enum)
            | flag(ACC_SYNTHETIC, self.is_compiler_generated)
    }
}

impl FromAccessBitfield for FieldProperties {
    fn verify(flags: u16) -> bool {
        if !Access::verify(flags) {
            return false;
        }

        if flags & ACC_FINAL != 0 && flags & ACC_VOLATILE != 0 {
            return false;
        }

        true
    }

    fn from_bitfield_unchecked(flags: u16) -> Self {
        FieldProperties {
            access: Access::from_bitfield_unchecked(flags),
            is_final: flags & ACC_FINAL != 0,
            is_static: flags & ACC_STATIC != 0,
            is_volatile: flags & ACC_VOLATILE != 0,
            is_transient: flags & ACC_TRANSIENT != 0,
            is_enum: flags & ACC_ENUM != 0,
            is_compiler_generated: flags & ACC_SYNTHETIC != 0,
        }
    }
}

/// Method properties.
///
/// Interface methods must have their `public` and `abstract` flags set. The may
/// have any of the `bridge`, `varargs`, or `synthetic` flags set.
///
/// Instance initialization methods may have at most one visibility modifier
/// set, as well as any of the `varargs`, `synthetic`, or `strict` flags. It
/// must have no other flags than these set.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct MethodProperties {
    pub access: Access,
    pub is_final: bool,
    pub is_static: bool,
    pub is_abstract: bool,
    pub is_synchronized: bool,
    pub is_native: bool,
    pub is_strict: bool,
    pub is_bridge: bool,
    pub is_varadic: bool,
    pub is_compiler_generated: bool,
}

impl ToAccessBitfield for MethodProperties {
    fn to_bitfield(self) -> AccessFlags {
        self.access.to_bitfield()
            | flag(ACC_FINAL, self.is_final)
            | flag(ACC_STATIC, self.is_static)
            | flag(ACC_ABSTRACT, self.is_abstract)
            | flag(ACC_SYNCHRONIZED, self.is_synchronized)
            | flag(ACC_NATIVE, self.is_native)
            | flag(ACC_STRICT, self.is_strict)
            | flag(ACC_BRIDGE, self.is_bridge)
            | flag(ACC_VARARGS, self.is_varadic)
            | flag(ACC_SYNTHETIC, self.is_compiler_generated)
    }
}

impl FromAccessBitfield for MethodProperties {
    fn verify(flags: u16) -> bool {
        if !Access::verify(flags) {
            return false;
        }

        const ABSTRACT_CONFLICTS: u16 =
            ACC_FINAL | ACC_NATIVE | ACC_PRIVATE | ACC_STATIC | ACC_STRICT | ACC_SYNCHRONIZED;

        if flags & ACC_ABSTRACT != 0 && flags & ABSTRACT_CONFLICTS != 0 {
            return false;
        }

        true
    }

    fn from_bitfield_unchecked(flags: u16) -> Self {
        MethodProperties {
            access: Access::from_bitfield_unchecked(flags),
            is_final: flags & ACC_FINAL != 0,
            is_static: flags & ACC_STATIC != 0,
            is_abstract: flags & ACC_ABSTRACT != 0,
            is_synchronized: flags & ACC_SYNCHRONIZED != 0,
            is_native: flags & ACC_NATIVE != 0,
            is_strict: flags & ACC_STRICT != 0,
            is_bridge: flags & ACC_BRIDGE != 0,
            is_varadic: flags & ACC_VARARGS != 0,
            is_compiler_generated: flags & ACC_SYNTHETIC != 0,
        }
    }
}

/// Marked or implicitly public in source.
pub const ACC_PUBLIC: u16 = 0x0001;
/// Marked private in source.
pub const ACC_PRIVATE: u16 = 0x0002;
/// Marked protected in source.
pub const ACC_PROTECTED: u16 = 0x0004;
/// Is an interface, not a class.
pub const ACC_INTERFACE: u16 = 0x0200;
/// Declared as an annotation type.
pub const ACC_ANNOTATION: u16 = 0x2000;
/// Declared as an enum type.
pub const ACC_ENUM: u16 = 0x4000;
/// Declared static.
pub const ACC_STATIC: u16 = 0x0008;
/// Declared final; must not be overridden (§5.4.5).
pub const ACC_FINAL: u16 = 0x0010;
/// Declared abstract; no implementation is provided.
pub const ACC_ABSTRACT: u16 = 0x0400;
/// Declared volatile; cannot be cached.
pub const ACC_VOLATILE: u16 = 0x0040;
/// Declared transient; not written or read by a persistent object manager.
pub const ACC_TRANSIENT: u16 = 0x0080;
/// Treat superclass methods specially when invoked by the invokespecial
/// instruction.
pub const ACC_SUPER: u16 = 0x0020;
/// Declared synchronized; invocation is wrapped by a monitor use.
pub const ACC_SYNCHRONIZED: u16 = 0x0020;
/// Declared with variable number of arguments.
pub const ACC_VARARGS: u16 = 0x0080;
/// Declared native; implemented in a language other than Java.
pub const ACC_NATIVE: u16 = 0x0100;
/// Declared strictfp; floating-point mode is FP-strict.
pub const ACC_STRICT: u16 = 0x0800;
/// A bridge method, generated by the compiler.
pub const ACC_BRIDGE: u16 = 0x0040;
/// Declared synthetic; not present in the source code.
pub const ACC_SYNTHETIC: u16 = 0x1000;
