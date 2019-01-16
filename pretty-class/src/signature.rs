use crate::APP;
use ::class::{
    access::ClassType,
    constant::Constant,
    field::{self, BaseType, FieldType},
    method::{self, ReturnDescriptor},
    parse::ByteParser,
    signature::*,
    ClassError, ClassResult,
};
use class::class::Class;

pub fn parse_unqualified_name(input: &mut ByteParser<'_>) -> ClassResult<String> {
    input.backtrace(|input| {
        let len = input.predicate_len(|ch| match ch {
            b'.' | b';' | b'[' | b'/' => false,
            _ => true,
        });
        Ok(::class::parse::parse_mutf8(input.take(len)?)?.into())
    })
}

// package_segment := <unqualifed_name> "/" ;
// binary_name := <package_segment>* <unqualifed_name> ;
pub fn parse_binary_package_segment(input: &mut ByteParser<'_>) -> ClassResult<String> {
    input.backtrace(|input| {
        let name = parse_unqualified_name(input)?;
        input.expect(b"/")?;
        Ok(name)
    })
}

pub fn parse_binary_name(input: &mut ByteParser<'_>) -> ClassResult<Vec<String>> {
    input.backtrace(|input| {
        let mut segments = input.repeat0(parse_binary_package_segment);
        segments.push(parse_unqualified_name(input)?);
        Ok(segments)
    })
}

pub fn field_desc_to_ty(field: field::Descriptor) -> ClassResult<Type> {
    Ok(match (field.dimensions, field.ty) {
        (0, FieldType::Primitive(prim)) => Type::Primitive(prim),
        (dim, FieldType::Primitive(prim)) => {
            Type::Reference(ReferenceType::Array(dim, Box::new(Type::Primitive(prim))))
        }

        (0, FieldType::Object(name)) => {
            let mut parser = ByteParser::new(name.as_bytes());
            let segments = parse_binary_name(&mut parser)?;

            Type::Reference(ReferenceType::Object(Box::new(object_type_from_path(
                segments,
            ))))
        }

        (dim, FieldType::Object(name)) => {
            let mut parser = ByteParser::new(name.as_bytes());
            let segments = parse_binary_name(&mut parser)?;

            Type::Reference(ReferenceType::Array(
                dim,
                Box::new(Type::Reference(ReferenceType::Object(Box::new(
                    object_type_from_path(segments),
                )))),
            ))
        }
    })
}

pub fn method_desc_to_sig(method: method::Descriptor) -> ClassResult<MethodSignature> {
    let args = Vec::from(method.args)
        .into_iter()
        .map(field_desc_to_ty)
        .collect::<Result<_, _>>()?;

    let ret = match method.ret {
        ReturnDescriptor::Void => None,
        ReturnDescriptor::Type(fd) => Some(field_desc_to_ty(fd)?),
    };

    Ok(MethodSignature {
        args,
        type_params: None,
        ret,
        throws: vec![].into(),
    })
}

fn object_type_from_path(mut path: Vec<String>) -> ObjectType {
    ObjectType::from_ident(path.pop().unwrap()).with_package_specifier(path.into_iter())
}

fn get_class_name(class: &Class, name: usize) -> Option<&str> {
    match &class.pool[name] {
        Constant::Class(idx) => class.pool[*idx].as_string_data(),
        _ => None,
    }
}

pub fn class_to_class_sig(class: &Class) -> ClassResult<ClassSignature> {
    let extends_path = parse_binary_name(&mut ByteParser::new(
        get_class_name(&class, class.super_class)
            .ok_or(ClassError::InvalidPoolType)?
            .as_bytes(),
    ))?;

    let extends = object_type_from_path(extends_path);

    let implements = class
        .interfaces
        .iter()
        .map(|idx| {
            Ok(object_type_from_path(parse_binary_name(
                &mut ByteParser::new(
                    get_class_name(&class, *idx)
                        .ok_or(ClassError::InvalidPoolType)?
                        .as_bytes(),
                ),
            )?))
        })
        .collect::<Result<_, ClassError>>()?;

    Ok(ClassSignature {
        type_params: None,
        extends,
        implements,
    })
}

fn extends_invisible(ty: &ObjectType) -> bool {
    ty.package
        .as_ref()
        .map(|path| {
            let ident = &*ty.class.ident;
            path.iter().zip(&["java", "lang"]).all(|(a, b)| a == b)
                && (ident == "Object" || ident == "Enum")
        })
        .unwrap_or(false)
}

pub fn print_class(sig: &ClassSignature, class: &Class) {
    if let Some(name) = get_class_name(class, class.this_class) {
        if let Ok(mut segments) = parse_binary_name(&mut ByteParser::new(name.as_bytes())) {
            let name = segments.pop().unwrap();
            print_path(&segments);
            APP.paint("type.object", || print!("{}", name));
        } else {

        }
    } else {
        print!("<invalid class name>");
    }

    if let Some(tys) = sig.type_params.as_ref() {
        print!("<");
        print_type_parameter(&tys[0]);
        for ty in &tys[1..] {
            print!(", ");
            print_type_parameter(ty);
        }
        print!(">");
    }

    if !extends_invisible(&sig.extends) {
        APP.paint("extends", || print!(" extends "));
        print_object_type(&sig.extends);
    }

    if sig.implements.len() > 0 {
        if class.properties.ty == ClassType::Interface {
            APP.paint("extends", || print!(" extends "));
        } else {
            APP.paint("extends", || print!(" implements "));
        }
        print_object_type(&sig.implements[0]);
        for ty in &sig.implements[1..] {
            print!(", ");
            print_object_type(ty);
        }
    }
}

pub fn print_method(sig: &MethodSignature, name: &str) {
    if let Some(tys) = sig.type_params.as_ref() {
        print!("<");
        print_type_parameter(&tys[0]);
        for ty in &tys[1..] {
            print!(", ");
            print_type_parameter(ty);
        }
        print!("> ");
    }

    if let Some(ty) = sig.ret.as_ref() {
        print_type(ty);
        print!(" ");
    } else {
        APP.paint("type.primitive.void", || print!("void "));
    }

    print!("{}(", name);
    if sig.args.len() > 0 {
        print_type(&sig.args[0]);
        for ty in &sig.args[1..] {
            print!(", ");
            print_type(ty);
        }
    }
    print!(")");

    if sig.throws.len() > 0 {
        APP.paint("throws", || print!(" throws "));
        print_throws(&sig.throws[0]);
        for ty in &sig.throws[1..] {
            print!(", ");
            print_throws(ty);
        }
    }
}

fn print_throws(throws: &Throws) {
    match throws {
        Throws::Object(ty) => print_object_type(ty),
        Throws::TypeVariable(ident) => {
            APP.paint("type.object.parameter", || print!("{}", ident));
        }
    }
}

pub fn print_type_parameter(param: &TypeParameter) {
    APP.paint("type.object.parameter", || print!("{}", param.ident));

    let mut iter = param
        .class_bound
        .as_ref()
        .into_iter()
        .chain(param.interface_bounds.iter())
        .filter(|ty| match ty {
            ReferenceType::Object(ty) => !extends_invisible(&ty),
            _ => true,
        });

    if let Some(ty) = iter.next() {
        APP.paint("extends", || print!(" extends "));
        print_reference_type(ty);
    }

    for ty in iter {
        print!(" & ");
        print_reference_type(ty);
    }
}

pub fn print_reference_type(sig: &ReferenceType) {
    match sig {
        ReferenceType::Object(ty) => print_object_type(ty),
        ReferenceType::Array(dimension, ty) => {
            print_type(ty);
            for _ in 0..*dimension {
                print!("[]");
            }
        }
        ReferenceType::TypeVariable(ident) => {
            APP.paint("type.object.parameter", || print!("{}", ident));
        }
    }
}

fn print_path(segments: &[String]) {
    if segments.len() > 0 && !APP.args.no_show_paths {
        for segment in segments {
            APP.paint("path", || print!("{}", segment));
            print!(".");
        }
    }
}

pub fn print_object_type(sig: &ObjectType) {
    if let Some(segments) = &sig.package {
        print_path(&segments);
    }

    print_object_type_fragment(&sig.class);

    if sig.inner.len() > 0 {
        for class in &sig.inner[..] {
            print!(".");
            print_object_type_fragment(class);
        }
    }
}

pub fn print_object_type_fragment(sig: &ObjectTypeFragment) {
    APP.paint("type.object", || print!("{}", &sig.ident));
    if let Some(ty_args) = &sig.type_arguments {
        print!("<");
        if ty_args.len() > 0 {
            print_type_argument(&ty_args[0]);
            for arg in &ty_args[1..] {
                print!(", ");
                print_type_argument(arg);
            }
        }
        print!(">");
    }
}

pub fn print_type_argument(sig: &TypeArgument) {
    match sig {
        TypeArgument::Wildcard => APP.paint("type.object.wildcard", || print!("?")),
        TypeArgument::Bounded(WildcardBound::Extends, ty) => {
            APP.paint("type.object.wildcard", || print!("? "));
            APP.paint("extends", || print!("extends "));
            print_reference_type(ty);
        }
        TypeArgument::Bounded(WildcardBound::Super, ty) => {
            APP.paint("type.object.wildcard", || print!("? "));
            APP.paint("super", || print!("super "));
            print_reference_type(ty);
        }
        TypeArgument::Exact(ty) => print_reference_type(ty),
    }
}

pub fn print_type(sig: &Type) {
    match sig {
        Type::Reference(ty) => print_reference_type(ty),
        Type::Primitive(ty) => print_base_type(ty),
    }
}

pub fn print_base_type(ty: &BaseType) {
    use ::class::field::BaseType;
    match ty {
        BaseType::Byte => APP.paint("type.primitive.byte", || print!("byte")),
        BaseType::Char => APP.paint("type.primitive.char", || print!("char")),
        BaseType::Double => APP.paint("type.primitive.double", || print!("double")),
        BaseType::Float => APP.paint("type.primitive.float", || print!("float")),
        BaseType::Int => APP.paint("type.primitive.int", || print!("int")),
        BaseType::Long => APP.paint("type.primitive.long", || print!("long")),
        BaseType::Short => APP.paint("type.primitive.short", || print!("short")),
        BaseType::Boolean => APP.paint("type.primitive.boolean", || print!("boolean")),
    }
}
