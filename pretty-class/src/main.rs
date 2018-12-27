use ::class::{
    attribute::{Attribute, AttributeInfo},
    class::{self, Class},
    constant::Constant,
    field, method,
};
use std::{collections::HashSet, fs::File, io::Read};

fn pad(count: usize) {
    for _ in 0..count {
        print!("  ");
    }
}

fn get_pool_str(pool: &[Constant], index: usize) -> &str {
    std::str::from_utf8(
        pool[index]
            .as_string_data()
            .unwrap_or(b"<string symbol not found>"),
    )
    .unwrap_or("<not utf8>")
}

fn get_pool_class_name(pool: &[Constant], index: usize) -> &str {
    match &pool[index] {
        &Constant::Class(index) => get_pool_str(pool, index),
        _ => "<class name not string>",
    }
}

fn print_attribute(class: &Class, attr: &AttributeInfo, depth: usize) {
    pad(depth);
    print!(
        "{}// {}{}: ",
        styles::COMMENT.prefix(),
        match &attr.attr {
            Attribute::Other(_) => "?",
            _ => " ",
        },
        get_pool_str(&class.pool, attr.name)
    );

    match &attr.attr {
        Attribute::Other(bytes) => {
            for b in &**bytes {
                print!("{:02x} ", b);
            }
        }

        Attribute::Code(code) => {
            for b in &*code.code {
                print!("{:02x} ", b);
            }
        }

        Attribute::StackMapTable(_) => print!("StackMapTable"),
        Attribute::ConstantValue(idx) => print!("{:?}", &class.pool[*idx]),
        Attribute::Signature(idx) => print!("{}", get_pool_str(&class.pool, *idx)),
    }
    println!("{}", styles::COMMENT.suffix());
}

use ansi_term::{Color, Style};

const fn fg(style: Style, color: Color) -> Style {
    Style {
        foreground: Some(color),
        ..style
    }
}

const fn bold(style: Style) -> Style {
    Style {
        is_bold: true,
        ..style
    }
}

const DATA_GROUP_STYLE: Style = bold(fg(styles::DEFAULT, Color::Green));
const INT_STYLE: Style = DATA_GROUP_STYLE;
const LONG_STYLE: Style = DATA_GROUP_STYLE;
const FLOAT_STYLE: Style = DATA_GROUP_STYLE;
const DOUBLE_STYLE: Style = DATA_GROUP_STYLE;
const STRING_STYLE: Style = styles::DEFAULT;

// Invocation and field metadata
const CLASS_REFERENCE_GROUP_STYLE: Style = fg(bold(styles::DEFAULT), Color::Yellow);
const FIELD_REF_STYLE: Style = CLASS_REFERENCE_GROUP_STYLE;
const METHOD_REF_STYLE: Style = CLASS_REFERENCE_GROUP_STYLE;
const INTERFACE_METHOD_REF_STYLE: Style = CLASS_REFERENCE_GROUP_STYLE;
const STRING_REF_STYLE: Style = CLASS_REFERENCE_GROUP_STYLE;
const CLASS_REF_STYLE: Style = CLASS_REFERENCE_GROUP_STYLE;

const NAME_AND_TYPE_STYLE: Style = fg(bold(styles::DEFAULT), Color::Blue);
const METHOD_TYPE_STYLE: Style = fg(styles::DEFAULT, Color::Purple);
const METHOD_HANDLE_STYLE: Style = fg(styles::DEFAULT, Color::Purple);
const INVOKE_DYNAMIC_STYLE: Style = fg(styles::DEFAULT, Color::Purple);

mod styles {
    use super::*;

    pub const DEFAULT: Style = Style {
        foreground: None,
        background: None,
        is_bold: false,
        is_dimmed: false,
        is_italic: false,
        is_underline: false,
        is_blink: false,
        is_reverse: false,
        is_hidden: false,
        is_strikethrough: false,
    };

    pub const ACCESS_VISIBILITY_GROUP: Style = fg(DEFAULT, Color::Yellow);
    pub const ACCESS_PUBLIC: Style = ACCESS_VISIBILITY_GROUP;
    pub const ACCESS_PROTECTED: Style = ACCESS_VISIBILITY_GROUP;
    pub const ACCESS_PRIVATE: Style = ACCESS_VISIBILITY_GROUP;

    pub const ACCESS_OTHER_QUALIFIER_GROUP: Style = fg(DEFAULT, Color::Yellow);
    // Method and field
    pub const ACCESS_FINAL: Style = ACCESS_OTHER_QUALIFIER_GROUP;
    pub const ACCESS_STATIC: Style = ACCESS_OTHER_QUALIFIER_GROUP;
    pub const ACCESS_ABSTRACT: Style = ACCESS_OTHER_QUALIFIER_GROUP;

    // Field only
    pub const ACCESS_VOLATILE: Style = ACCESS_OTHER_QUALIFIER_GROUP;
    pub const ACCESS_TRANSIENT: Style = ACCESS_OTHER_QUALIFIER_GROUP;

    // Method only
    pub const ACCESS_SYNCHRONIZED: Style = ACCESS_OTHER_QUALIFIER_GROUP;
    pub const ACCESS_STRICTFP: Style = ACCESS_OTHER_QUALIFIER_GROUP;
    pub const ACCESS_NATIVE: Style = ACCESS_OTHER_QUALIFIER_GROUP;

    pub const ACCESS_CLASS_TYPE: Style = fg(bold(DEFAULT), Color::Blue);
    pub const ACCESS_ENUM: Style = ACCESS_CLASS_TYPE;
    pub const ACCESS_INTERFACE: Style = ACCESS_CLASS_TYPE;
    pub const ACCESS_CLASS: Style = ACCESS_CLASS_TYPE;
    pub const ACCESS_ANNOTATION: Style = ACCESS_CLASS_TYPE;

    pub const COMMENT: Style = fg(DEFAULT, Color::Cyan);
    pub const INDEX: Style = fg(bold(DEFAULT), Color::Cyan);

    pub const PRIMITIVE_TYPE: Style = fg(DEFAULT, Color::Green);
    pub const CLASS_NAME: Style = bold(PRIMITIVE_TYPE);
    pub const EXTENDS: Style = fg(bold(DEFAULT), Color::Yellow);
}

fn print_class_access(access: &class::Access) {
    let mut was_written = false;
    let mut write = |s, col: Style| {
        if was_written {
            print!(" {}", col.paint(s));
        } else {
            print!("{}", col.paint(s));
            was_written = true;
        }
    };

    if access.is(class::Access::PUBLIC) {
        write("public", styles::ACCESS_PUBLIC);
    }

    if access.is(class::Access::ENUM) {
        write("enum", styles::ACCESS_ENUM);
    } else if access.is(class::Access::ANNOTATION) {
        write("@interface", styles::ACCESS_ANNOTATION);
    } else if access.is(class::Access::INTERFACE) {
        write("interface", styles::ACCESS_INTERFACE);
    } else {
        if access.is(class::Access::FINAL) {
            write("final", styles::ACCESS_FINAL);
        } else if access.is(class::Access::ABSTRACT) {
            write("abstract", styles::ACCESS_ABSTRACT);
        }

        write("class", styles::ACCESS_CLASS);
    }
}

fn print_field_access(access: &field::Access) {
    let mut was_written = false;
    let mut write = |s, col: Style| {
        if was_written {
            print!(" {}", col.paint(s));
        } else {
            print!("{}", col.paint(s));
            was_written = true;
        }
    };

    if access.is(field::Access::PUBLIC) {
        write("public", styles::ACCESS_PUBLIC);
    } else if access.is(field::Access::PROTECTED) {
        write("protected", styles::ACCESS_PROTECTED);
    } else if access.is(field::Access::PRIVATE) {
        write("private", styles::ACCESS_PRIVATE);
    }

    if access.is(field::Access::STATIC) {
        write("static", styles::ACCESS_STATIC);
    }

    if access.is(field::Access::FINAL) {
        write("final", styles::ACCESS_FINAL);
    } else if access.is(field::Access::TRANSIENT) {
        write("transient", styles::ACCESS_TRANSIENT);
    }

    if access.is(field::Access::VOLATILE) {
        write("volatile", styles::ACCESS_VOLATILE);
    }

    if was_written {
        print!(" ");
    }
}

fn print_method_access(access: &method::Access) {
    let mut was_written = false;
    let mut write = |s, col: Style| {
        if was_written {
            print!(" {}", col.paint(s));
        } else {
            print!("{}", col.paint(s));
            was_written = true;
        }
    };

    if access.is(method::Access::PUBLIC) {
        write("public", styles::ACCESS_PUBLIC);
    } else if access.is(method::Access::PROTECTED) {
        write("protected", styles::ACCESS_PROTECTED);
    } else if access.is(method::Access::PRIVATE) {
        write("private", styles::ACCESS_PRIVATE);
    }

    if access.is(method::Access::ABSTRACT) {
        write("abstract", styles::ACCESS_ABSTRACT);
    } else {
        if access.is(method::Access::STATIC) {
            write("static", styles::ACCESS_STATIC);
        }

        if access.is(method::Access::FINAL) {
            write("final", styles::ACCESS_FINAL);
        }

        if access.is(method::Access::SYNCHRONIZED) {
            write("synchronized", styles::ACCESS_SYNCHRONIZED);
        }

        if access.is(method::Access::STRICT) {
            write("strictfp", styles::ACCESS_STRICTFP);
        }

        if access.is(method::Access::NATIVE) {
            write("native", styles::ACCESS_NATIVE);
        }
    }

    if was_written {
        print!(" ");
    }
}

fn print_field_descriptor(descriptor: &field::Descriptor) {
    match &descriptor.ty {
        field::FieldType::Byte => print!("{}", styles::PRIMITIVE_TYPE.paint("byte")),
        field::FieldType::Char => print!("{}", styles::PRIMITIVE_TYPE.paint("char")),
        field::FieldType::Double => print!("{}", styles::PRIMITIVE_TYPE.paint("double")),
        field::FieldType::Float => print!("{}", styles::PRIMITIVE_TYPE.paint("float")),
        field::FieldType::Int => print!("{}", styles::PRIMITIVE_TYPE.paint("int")),
        field::FieldType::Long => print!("{}", styles::PRIMITIVE_TYPE.paint("long")),
        field::FieldType::Short => print!("{}", styles::PRIMITIVE_TYPE.paint("short")),
        field::FieldType::Boolean => print!("{}", styles::PRIMITIVE_TYPE.paint("boolean")),
        field::FieldType::Object(name) => {
            let mut iter = std::str::from_utf8(&name)
                .unwrap_or("<not utf8>")
                .split("/");

            print!("{}", styles::CLASS_NAME.paint(iter.next().unwrap()));

            for item in iter {
                print!(".{}", styles::CLASS_NAME.paint(item));
            }
        }
    }

    for _ in 0..descriptor.dimensions {
        print!("[]");
    }
}

fn print_method_descriptor(descriptor: &method::Descriptor, name: &str) {
    match &descriptor.ret {
        method::ReturnDescriptor::Void => print!("{}", styles::PRIMITIVE_TYPE.paint("void")),
        method::ReturnDescriptor::Type(f) => print_field_descriptor(f),
    }

    print!(" {}(", name);

    if descriptor.args.len() > 0 {
        print_field_descriptor(&descriptor.args[0]);
        for item in &descriptor.args[1..] {
            print!(", ");
            print_field_descriptor(item);
        }
    }

    print!(")");
}

fn print_constant(pool: &[Constant], index: usize, depth: usize) {
    pad(depth);

    fn pad_constant_name(name: &str, color: Style) {
        print!("{}: ", color.paint(name));
        for _ in 0..("InterfaceMethodRef".len() - name.len()) {
            print!(" ");
        }
    }

    match &pool[index] {
        Constant::Nothing => println!("<nothing>"),
        Constant::Integer(val) => {
            pad_constant_name("Int", INT_STYLE);
            println!("{}", val)
        }
        Constant::Float(val) => {
            pad_constant_name("Float", FLOAT_STYLE);
            println!("{}", val)
        }
        Constant::Long(val) => {
            pad_constant_name("Long", LONG_STYLE);
            println!("{}", val)
        }
        Constant::Double(val) => {
            pad_constant_name("Double", DOUBLE_STYLE);
            println!("{}", val)
        }

        Constant::StringData(data) => {
            pad_constant_name("String", STRING_STYLE);
            println!("{}", std::str::from_utf8(data).unwrap_or("<not utf8>"))
        }

        Constant::String(idx) => {
            pad_constant_name("StringRef", STRING_REF_STYLE);
            println!(
                "{}#{}{} {}// {}{}",
                styles::INDEX.prefix(),
                idx,
                styles::INDEX.suffix(),
                styles::COMMENT.prefix(),
                get_pool_str(pool, *idx),
                styles::COMMENT.suffix()
            );
        }

        Constant::Class(idx) => {
            pad_constant_name("ClassRef", CLASS_REF_STYLE);
            println!(
                "{}#{}{} {}// {}{}",
                styles::INDEX.prefix(),
                idx,
                styles::INDEX.suffix(),
                styles::COMMENT.prefix(),
                get_pool_str(pool, *idx),
                styles::COMMENT.suffix()
            );
        }

        Constant::MethodType(idx) => {
            pad_constant_name("MethodType", METHOD_TYPE_STYLE);
            println!(
                "{}#{}{} {}// {}{}",
                styles::INDEX.prefix(),
                idx,
                styles::INDEX.suffix(),
                styles::COMMENT.prefix(),
                get_pool_str(pool, *idx),
                styles::COMMENT.suffix()
            );
        }

        Constant::FieldRef {
            class,
            name_and_type,
        } => {
            pad_constant_name("FieldRef", FIELD_REF_STYLE);
            match (&pool[*class], &pool[*name_and_type]) {
                (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                    println!(
                        "{}#{} #{}{} {}// {}.{}:{}{}",
                        styles::INDEX.prefix(),
                        class,
                        name_and_type,
                        styles::INDEX.suffix(),
                        styles::COMMENT.prefix(),
                        get_pool_str(pool, *class_idx),
                        get_pool_str(pool, *name),
                        get_pool_str(pool, *ty),
                        styles::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid field ref>"),
            }
        }

        Constant::MethodRef {
            class,
            name_and_type,
        } => {
            pad_constant_name("MethodRef", METHOD_REF_STYLE);
            match (&pool[*class], &pool[*name_and_type]) {
                (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                    println!(
                        "{}#{} #{}{} {}// {}.{}:{}{}",
                        styles::INDEX.prefix(),
                        class,
                        name_and_type,
                        styles::INDEX.suffix(),
                        styles::COMMENT.prefix(),
                        get_pool_str(pool, *class_idx),
                        get_pool_str(pool, *name),
                        get_pool_str(pool, *ty),
                        styles::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid method ref>"),
            }
        }

        Constant::InterfaceMethodRef {
            class,
            name_and_type,
        } => {
            pad_constant_name("InterfaceMethodRef", INTERFACE_METHOD_REF_STYLE);
            match (&pool[*class], &pool[*name_and_type]) {
                (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                    println!(
                        "{}#{} #{}{} {}// {}.{}:{}{}",
                        styles::INDEX.prefix(),
                        class,
                        name_and_type,
                        styles::INDEX.suffix(),
                        styles::COMMENT.prefix(),
                        get_pool_str(pool, *class_idx),
                        get_pool_str(pool, *name),
                        get_pool_str(pool, *ty),
                        styles::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid interface method ref>"),
            }
        }

        Constant::NameAndType { name, ty } => {
            pad_constant_name("NameAndType", NAME_AND_TYPE_STYLE);
            println!(
                "{}#{} #{}{} {}// {}:{}{}",
                styles::INDEX.prefix(),
                name,
                ty,
                styles::INDEX.suffix(),
                styles::COMMENT.prefix(),
                get_pool_str(pool, *name),
                get_pool_str(pool, *ty),
                styles::COMMENT.suffix(),
            );
        }

        Constant::MethodHandle { kind: _, index } => {
            pad_constant_name("MethodHandle", METHOD_HANDLE_STYLE);
            match &pool[*index] {
                Constant::MethodRef {
                    class,
                    name_and_type,
                } => match (&pool[*class], &pool[*name_and_type]) {
                    (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                        println!(
                            "{ip}#{a}{is} ({ip}#{b} #{c}{is}) {cp}// {d}.{e}:{f}{cs}",
                            ip = styles::INDEX.prefix(),
                            is = styles::INDEX.suffix(),
                            cp = styles::COMMENT.prefix(),
                            cs = styles::COMMENT.suffix(),
                            a = class,
                            b = name_and_type,
                            c = name_and_type,
                            d = get_pool_str(pool, *class_idx),
                            e = get_pool_str(pool, *name),
                            f = get_pool_str(pool, *ty),
                        );
                    }
                    _ => println!("<invalid method handle>"),
                },
                _ => println!("<invalid method handle>"),
            }
        }

        Constant::InvokeDynamic {
            bootstrap_method_attr,
            name_and_type,
        } => {
            pad_constant_name("InvokeDynamic", INVOKE_DYNAMIC_STYLE);
            match &pool[*name_and_type] {
                Constant::NameAndType { name, ty } => {
                    println!(
                        "#{} {}#{}{} {}// {}:{}{}",
                        bootstrap_method_attr,
                        styles::INDEX.prefix(),
                        name_and_type,
                        styles::INDEX.suffix(),
                        styles::COMMENT.prefix(),
                        get_pool_str(pool, *name),
                        get_pool_str(pool, *ty),
                        styles::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid invoke dynamic>"),
            }
        }
    }
}

fn print_class_decl(class: &Class) {
    print_class_access(&class.access_flags);
    print!(
        " {}",
        styles::CLASS_NAME.paint(get_pool_class_name(&class.pool, class.this_class))
    );

    match get_pool_class_name(&class.pool, class.super_class) {
        "java/lang/Object" | "java/lang/Enum" => {}
        _ => print!(
            " {} {}",
            styles::EXTENDS.paint("extends"),
            styles::CLASS_NAME.paint(get_pool_class_name(&class.pool, class.super_class))
        ),
    }

    if class.interfaces.len() > 0 {
        print!(" {} ", styles::EXTENDS.paint("implements"));
        // pprint_list(class.interfaces.iter().map(|idx|
        // get_pool_class_name(&class.pool, idx)), styles::CLASS_NAME);

        let len = class.interfaces.len();
        for &idx in &class.interfaces[..len - 1] {
            print!(
                "{}{}{}, ",
                styles::CLASS_NAME.prefix(),
                get_pool_class_name(&class.pool, idx),
                styles::CLASS_NAME.suffix()
            );
        }

        print!(
            "{}{}{}",
            styles::CLASS_NAME.prefix(),
            get_pool_class_name(&class.pool, class.interfaces[len - 1]),
            styles::CLASS_NAME.suffix()
        );
    }
}

fn main() {
    let mut buf = Vec::new();

    let mut file_path = None;
    let mut short_set = HashSet::new();
    let mut long_set = HashSet::new();

    for mut arg in std::env::args().skip(1) {
        if arg.starts_with("-") {
            if arg.starts_with("--") {
                long_set.insert(arg.split_off(2));
            } else {
                short_set.extend(arg.chars().skip(1));
            }
        } else {
            file_path = Some(arg);
        }
    }

    let mut file = File::open(file_path.unwrap()).unwrap();
    file.read_to_end(&mut buf).unwrap();

    let class = Class::parse(buf).unwrap();

    if short_set.contains(&'c') || long_set.contains("constant-pool") {
        for entry in 0..class.pool.len() {
            print!(
                "{}{:5}{} = ",
                styles::INDEX.prefix(),
                entry,
                styles::INDEX.suffix()
            );
            print_constant(&class.pool, entry, 0);
        }
    } else {
        for attr in &*class.attributes {
            print_attribute(&class, &attr, 0);
        }
        println!(
            "{}// Version {}.{}{}",
            styles::COMMENT.prefix(),
            class.version.major,
            class.version.minor,
            styles::COMMENT.suffix()
        );

        print_class_decl(&class);

        println!(" {{");

        for field in &*class.fields {
            for attr in &*field.attributes {
                print_attribute(&class, &attr, 1);
            }
            pad(1);
            print_field_access(&field.access);
            let name = get_pool_str(&class.pool, field.name);

            print_field_descriptor(&field.descriptor);
            println!(" {};", name);
        }

        println!();

        for method in &*class.methods {
            for attr in &*method.attributes {
                print_attribute(&class, &attr, 1);
            }

            pad(1);
            print_method_access(&method.access);
            let name = get_pool_str(&class.pool, method.name);

            print_method_descriptor(&method.descriptor, name);
            println!(";");
        }
        println!("}}");
        // println!("{:?}", class);
    }
}

// fn pprint_list<T, I>(mut iter: I, style: Style) {
//     if iter.len() > 0 {
//         iter.take(iter.len() - 1)
//             .for_each(|item| print!("{}{}{}, ", style.prefix(), item,
// style.suffix()));         print!(
//             "{}{}{}",
//             style.prefix(),
//             iter.next().unwrap(),
//             style.suffix()
//         );
//     }
// }
