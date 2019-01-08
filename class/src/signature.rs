use crate::{
    field::BaseType,
    parse::{self, ByteParser},
    ClassError, ClassResult,
};

/// Represents some type, whether it is a reference type or a primitive type
/// like `int`.
///
/// ```txt
/// type := <primitive_type> | <reference_type> ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Type {
    Primitive(BaseType),
    Reference(ReferenceType),
}

/// Represents some reference type.
///
/// ```txt
/// array_type     := "[" <type> ;
/// type_variable  := "T" <ident> ";" ;
/// reference_type := <class_type_signature>
///                 | <array_type>
///                 | <type_variable>
///                 ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ReferenceType {
    Object(Box<ObjectType>),
    Array(usize, Box<Type>),
    /// Reference to a type variable.
    TypeVariable(String),
}

/// Represents a single type parameter.
///
/// ```txt
/// type_param      := <ident> <class_bound> <interface_bound>* ;
/// class_bound     := ":" <reference_type>? ;
/// interface_bound := ":" <reference_type> ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeParameter {
    /// The name of the type paramater, something like `T`
    pub ident: String,
    /// The superclass bound, the `A` in `<T extends A>` where `A` is a class.
    pub class_bound: Option<ReferenceType>,
    /// The superinterface bounds, `B1` and `B2` in `<T extends B1 & B2>` where
    /// `B1` and `B2` are interfaces.
    pub interface_bounds: Box<[ReferenceType]>,
}

/// Represents a fully elaborated class.
///
/// This represents an object type. It has the full package specifier, the
/// furthest outer class, and all the inner classes between "this" and the
/// outer. "This" class is either the last item of `inner`, or `class`, if
/// `inner` is empty.
///
/// ```txt
/// package_specifier  := <ident> "/" <package_specifier>? ;
/// object_type_suffix := "." <object_type_fragment>;
/// object_type        := "L" <package_specifier>? <object_type_fragment> <object_type_suffix>* ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ObjectType {
    /// The segments of the package that this class is in. This value for
    /// `java.lang.String` would be `["java", "lang"]`.
    pub package: Option<Box<[String]>>,
    /// The outer class.
    pub class: ObjectTypeFragment,
    /// All of the classes between `class` and the last. eg, `Foo<A, B,
    /// C>.Bar<A, C>.Baz` would be `[Foo<A, B, C>, Bar<A, C>, Baz]`.
    pub inner: Box<[ObjectTypeFragment]>,
}

impl ObjectType {
    /// Creates an object type from a string representing the base name. This
    /// will not have any package specifier, and will not be an inner class.
    pub fn from_ident(class: String) -> Self {
        ObjectType {
            package: None,
            inner: Default::default(),
            class: ObjectTypeFragment::from_ident(class),
        }
    }

    /// Specifies the package that this object type appears in. See struct
    /// documentation.
    pub fn with_package_specifier<I>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = String>,
    {
        ObjectType {
            package: Some(iter.into_iter().collect()),
            ..self
        }
    }
}

/// Represents a possibly parameterized class.
///
/// ```txt
/// type_arguments       := "<" <type_arguments>+ ">" ;
/// object_type_fragment := <ident> <type_arguments>? ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ObjectTypeFragment {
    /// The name of this class, without any package specifier.
    pub ident: String,
    /// The type arguments on this class.
    pub type_arguments: Option<Box<[TypeArgument]>>,
}

impl ObjectTypeFragment {
    /// Creates a fragment with no type arguments from a string representing the
    /// identifier.
    pub fn from_ident(ident: String) -> Self {
        ObjectTypeFragment {
            ident,
            type_arguments: None,
        }
    }
}

/// Represents a bound on a wildcard.
///
/// `Extends` means `<? extends T>`, and `Super` means `<? super T>`.
///
/// ```txt
/// wildcard_bound := "+" | "-" ;
/// ```
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum WildcardBound {
    Extends,
    Super,
}

/// Represents the use of a type parameter.
///
/// ```txt
/// type_argument := <wildcard_bound>? <reference_type>
///                | "*"
///                ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeArgument {
    /// `<?>`
    Wildcard,
    /// `<? extends B>` or `<? super B>`
    Bounded(WildcardBound, ReferenceType),
    /// `<T>`
    Exact(ReferenceType),
}

/// Represents the signature of a class; what type bounds it has, what it
/// extends, and what it implements.
///
/// ```txt
/// class_sig := <type_param>? <object_type> <object_type>* ;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ClassSignature {
    /// The type paramaters declared on this class.
    pub type_params: Option<Box<[TypeParameter]>>,
    /// The class that this class extends.
    pub extends: ObjectType,
    /// The interafaces that this class implements.
    pub implements: Box<[ObjectType]>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct MethodSignature {
    /// The type paramaters declared on this method.
    pub type_params: Option<Box<[TypeParameter]>>,
    pub args: Box<[Type]>,
    /// Either the return type of the method, or `void`, represented by `None`.
    pub ret: Option<Type>,
    pub throws: Box<[Throws]>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Throws {
    Object(ObjectType),
    TypeVariable(String),
}

/// Parse an identifier as specified in §4.3.4
pub fn parse_identifier(input: &mut ByteParser<'_>) -> ClassResult<String> {
    input.backtrace(|input| {
        Ok(parse::parse_mutf8(input.peeking_take_while(|ch| match ch {
            b'.' | b';' | b'[' | b'/' | b'<' | b'>' | b':' => false,
            _ => true,
        })?)?
        .into())
    })
}

// ClassSignature:
//   FormalTypeParametersopt SuperclassSignature SuperinterfaceSignature*
pub fn parse_class_signature(input: &mut ByteParser<'_>) -> ClassResult<ClassSignature> {
    input.backtrace(|input| {
        let type_params = parse_formal_type_parameters(input).ok();
        let extends = parse_class_type_signature(input)?;
        let implements = input.repeat0(parse_class_type_signature).into();

        Ok(ClassSignature {
            type_params: type_params.into(),
            extends,
            implements,
        })
    })
}

// FormalTypeParameters:
//   < FormalTypeParameter+ >
pub fn parse_formal_type_parameters(
    input: &mut ByteParser<'_>,
) -> ClassResult<Box<[TypeParameter]>> {
    input.backtrace(|input| {
        input.expect(b"<")?;
        let res = input.repeat1(parse_formal_type_parameter)?;
        input.expect(b">")?;

        Ok(res.into())
    })
}

// FormalTypeParameter:
//   Identifier ClassBound InterfaceBound*
pub fn parse_formal_type_parameter(input: &mut ByteParser<'_>) -> ClassResult<TypeParameter> {
    input.backtrace(|input| {
        let ident = parse_identifier(input)?;
        let class_bound = parse_class_bound(input)?;
        let interface_bounds = input.repeat0(parse_interface_bound);

        Ok(TypeParameter {
            ident,
            class_bound,
            interface_bounds: interface_bounds.into(),
        })
    })
}

// ClassBound:
//   : FieldTypeSignatureopt
// InterfaceBound:
//   : FieldTypeSignature
pub fn parse_class_bound(input: &mut ByteParser<'_>) -> ClassResult<Option<ReferenceType>> {
    input.backtrace(|input| {
        input.expect(b":")?;
        Ok(parse_field_type_signature(input).ok())
    })
}

// InterfaceBound:
//   : FieldTypeSignature
pub fn parse_interface_bound(input: &mut ByteParser<'_>) -> ClassResult<ReferenceType> {
    input.backtrace(|input| {
        input.expect(b":")?;
        parse_field_type_signature(input)
    })
}

// FieldTypeSignature:
//   ClassTypeSignature
//   ArrayTypeSignature
//   TypeVariableSignature
pub fn parse_field_type_signature(input: &mut ByteParser<'_>) -> ClassResult<ReferenceType> {
    if let Some(sig) = parse_class_type_signature(input).ok() {
        return Ok(ReferenceType::Object(Box::new(sig)));
    }

    if let Some((len, ty)) = parse_array_type_signature(input).ok() {
        return Ok(ReferenceType::Array(len, Box::new(ty)));
    }

    parse_type_variable_signature(input).map(ReferenceType::TypeVariable)
}

// SuperclassSignature:
//   ClassTypeSignature
// SuperinterfaceSignature:
//   ClassTypeSignature
//
// ClassTypeSignature:
//   L PackageSpecifieropt SimpleClassTypeSignature ClassTypeSignatureSuffix* ;
pub fn parse_class_type_signature(input: &mut ByteParser<'_>) -> ClassResult<ObjectType> {
    input.backtrace(|input| {
        input.expect(b"L")?;
        let package = parse_package_specifier(input).ok();
        let class = parse_simple_class_type_signature(input)?;
        let inner = input.repeat0(parse_class_type_signature_suffix).into();
        input.expect(b";")?;

        Ok(ObjectType {
            package: package.map(|v| v.into()),
            class,
            inner,
        })
    })
}

// PackageSpecifier:
//   Identifier / PackageSpecifier*
pub fn parse_package_specifier(input: &mut ByteParser<'_>) -> ClassResult<Vec<String>> {
    input.backtrace(|input| {
        let segment = parse_identifier(input)?;
        input.expect(b"/")?;
        let mut vec = vec![segment];
        vec.extend(parse_package_specifier(input).unwrap_or_default());
        Ok(vec)
    })
}

// SimpleClassTypeSignature:
//   Identifier TypeArgumentsopt
pub fn parse_simple_class_type_signature(
    input: &mut ByteParser<'_>,
) -> ClassResult<ObjectTypeFragment> {
    input.backtrace(|input| {
        let ident = parse_identifier(input)?;
        let type_arguments = parse_type_arguments(input).ok();
        Ok(ObjectTypeFragment {
            ident,
            type_arguments,
        })
    })
}

// ClassTypeSignatureSuffix:
//   . SimpleClassTypeSignature
pub fn parse_class_type_signature_suffix(
    input: &mut ByteParser<'_>,
) -> ClassResult<ObjectTypeFragment> {
    input.backtrace(|input| {
        input.expect(b".")?;
        parse_simple_class_type_signature(input)
    })
}

// TypeVariableSignature:
//   T Identifier ;
pub fn parse_type_variable_signature(input: &mut ByteParser<'_>) -> ClassResult<String> {
    input.backtrace(|input| {
        input.expect(b"T")?;
        let ident = parse_identifier(input)?;
        input.expect(b";")?;
        Ok(ident)
    })
}

// TypeArguments:
//   < TypeArgument+ >
pub fn parse_type_arguments(input: &mut ByteParser<'_>) -> ClassResult<Box<[TypeArgument]>> {
    input.backtrace(|input| {
        input.expect(b"<")?;
        let args = input.repeat1(parse_type_argument)?.into();
        input.expect(b">")?;
        Ok(args)
    })
}

// TypeArgument:
//   WildcardIndicatoropt FieldTypeSignature
//   *
pub fn parse_type_argument(input: &mut ByteParser<'_>) -> ClassResult<TypeArgument> {
    input.backtrace(|input| {
        Ok(match parse_wildcard_indicator(input).ok() {
            Some(bound) => TypeArgument::Bounded(bound, parse_field_type_signature(input)?),
            None => match input.expect(b"*").ok() {
                Some(_) => TypeArgument::Wildcard,
                None => TypeArgument::Exact(parse_field_type_signature(input)?),
            },
        })
    })
}

// WildcardIndicator:
//   +
//   -
pub fn parse_wildcard_indicator(input: &mut ByteParser<'_>) -> ClassResult<WildcardBound> {
    input.backtrace(|input| {
        Ok(match input.take(1)? {
            b"+" => WildcardBound::Extends,
            b"-" => WildcardBound::Super,
            k => return Err(ClassError::InvalidWildcardBound(k[0])),
        })
    })
}

// ArrayTypeSignature:
//   [ TypeSignature
pub fn parse_array_type_signature(input: &mut ByteParser<'_>) -> ClassResult<(usize, Type)> {
    // TODO: make this not recursive?
    input.backtrace(|input| {
        input.expect(b"[")?;
        Ok(match parse_type_signature(input)? {
            Type::Reference(ReferenceType::Array(len, ty)) => (len + 1, *ty),
            other => (1, other),
        })
    })
}

// TypeSignature:
//   FieldTypeSignature
//   BaseType
pub fn parse_type_signature(input: &mut ByteParser<'_>) -> ClassResult<Type> {
    input.backtrace(|input| {
        Ok(match parse_base_type(input).ok() {
            Some(ty) => Type::Primitive(ty),
            None => Type::Reference(parse_field_type_signature(input)?),
        })
    })
}

pub(crate) fn parse_base_type(input: &mut ByteParser<'_>) -> ClassResult<BaseType> {
    input.backtrace(|input| {
        Ok(match input.parse_u8()? {
            b'B' => BaseType::Byte,
            b'C' => BaseType::Char,
            b'D' => BaseType::Double,
            b'F' => BaseType::Float,
            b'I' => BaseType::Int,
            b'J' => BaseType::Long,
            b'S' => BaseType::Short,
            b'Z' => BaseType::Boolean,

            other => return Err(ClassError::InvalidBaseType(other)),
        })
    })
}

// MethodTypeSignature:
//     FormalTypeParametersopt (TypeSignature*) ReturnType ThrowsSignature*
pub fn parse_method_signature(input: &mut ByteParser<'_>) -> ClassResult<MethodSignature> {
    input.backtrace(|input| {
        let type_params = parse_formal_type_parameters(input).ok();
        input.expect(b"(")?;
        let args = input.repeat0(parse_type_signature).into();
        input.expect(b")")?;
        let ret = parse_return_type(input)?;
        let throws = input.repeat0(parse_throws_signature).into();

        Ok(MethodSignature {
            type_params,
            args,
            ret,
            throws,
        })
    })
}

// ReturnType:
//     TypeSignature
//     VoidDescriptor
pub fn parse_return_type(input: &mut ByteParser<'_>) -> ClassResult<Option<Type>> {
    input.backtrace(|input| {
        if let Ok(_) = input.expect(b"V") {
            return Ok(None);
        }

        parse_type_signature(input).map(Some)
    })
}

// ThrowsSignature:
//     ^ ClassTypeSignature
//     ^ TypeVariableSignature
pub fn parse_throws_signature(input: &mut ByteParser<'_>) -> ClassResult<Throws> {
    input.backtrace(|input| {
        input.expect(b"^")?;

        if let Ok(object) = parse_class_type_signature(input) {
            return Ok(Throws::Object(object));
        }

        parse_type_variable_signature(input).map(Throws::TypeVariable)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sig_method() {
        let input = b"<T::L/java/lang/Iterable;>(TT;[ILjava/lang/Object;)V^Ljava/io/IOException;";
        let mut p = ByteParser::new(input);

        let sig = parse_method_signature(&mut p);

        let type_params =
            parse_formal_type_parameters(&mut ByteParser::new(b"<T::L/java/lang/Iterable;>"))
                .unwrap();

        let arg1 = parse_type_signature(&mut ByteParser::new(b"TT;")).unwrap();
        let arg2 = parse_type_signature(&mut ByteParser::new(b"[I")).unwrap();
        let arg3 = parse_type_signature(&mut ByteParser::new(b"Ljava/lang/Object;")).unwrap();

        let ret = parse_return_type(&mut ByteParser::new(b"V")).unwrap();
        let throws =
            parse_throws_signature(&mut ByteParser::new(b"^Ljava/io/IOException;")).unwrap();

        assert_eq!(
            sig,
            Ok(MethodSignature {
                type_params: Some(type_params),
                args: vec![arg1, arg2, arg3].into(),
                ret,
                throws: vec![throws].into(),
            })
        )
    }

    #[test]
    fn test_sig_ident() {
        let input = b"one.two;three[four/five<six>seven:";
        let mut p = ByteParser::new(input);

        assert_eq!(Ok("one".into()), parse_identifier(&mut p));
        p.expect(b".").unwrap();
        assert_eq!(Ok("two".into()), parse_identifier(&mut p));
        p.expect(b";").unwrap();
        assert_eq!(Ok("three".into()), parse_identifier(&mut p));
        p.expect(b"[").unwrap();
        assert_eq!(Ok("four".into()), parse_identifier(&mut p));
        p.expect(b"/").unwrap();
        assert_eq!(Ok("five".into()), parse_identifier(&mut p));
        p.expect(b"<").unwrap();
        assert_eq!(Ok("six".into()), parse_identifier(&mut p));
        p.expect(b">").unwrap();
        assert_eq!(Ok("seven".into()), parse_identifier(&mut p));
        p.expect(b":").unwrap();
    }

    #[test]
    fn test_sig_object_type() {
        // net.xavil.MyObject<? extends T>[][]
        let input = b"[[Lnet/xavil/MyObject<+TT;>;";
        let mut p = ByteParser::new(input);

        let res = ReferenceType::Array(
            2,
            Box::new(Type::Reference(ReferenceType::Object(Box::new(
                ObjectType {
                    package: Some(vec!["net".into(), "xavil".into()].into()),
                    class: ObjectTypeFragment {
                        ident: "MyObject".into(),
                        type_arguments: Some(
                            vec![TypeArgument::Bounded(
                                WildcardBound::Extends,
                                ReferenceType::TypeVariable("T".into()),
                            )]
                            .into(),
                        ),
                    },
                    inner: Box::new([]),
                },
            )))),
        );

        assert_eq!(Ok(res), parse_field_type_signature(&mut p));
    }
}
