use crate::{pad, pool, style};
use ansi_term::Style;
use class::constant::Constant;

pub fn print_constant(pool: &[Constant], index: usize, depth: usize) {
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
            pad_constant_name("Int", style::INT);
            println!("{}", val)
        }
        Constant::Float(val) => {
            pad_constant_name("Float", style::FLOAT);
            println!("{}", val)
        }
        Constant::Long(val) => {
            pad_constant_name("Long", style::LONG);
            println!("{}", val)
        }
        Constant::Double(val) => {
            pad_constant_name("Double", style::DOUBLE);
            println!("{}", val)
        }

        Constant::StringData(data) => {
            pad_constant_name("String", style::STRING);
            println!("{}", std::str::from_utf8(data).unwrap_or("<not utf8>"))
        }

        Constant::String(idx) => {
            pad_constant_name("StringRef", style::STRING_REF);
            println!(
                "{}#{}{} {}// {}{}",
                style::INDEX.prefix(),
                idx,
                style::INDEX.suffix(),
                style::COMMENT.prefix(),
                pool::get_str(pool, *idx),
                style::COMMENT.suffix()
            );
        }

        Constant::Class(idx) => {
            pad_constant_name("ClassRef", style::CLASS_REF);
            println!(
                "{}#{}{} {}// {}{}",
                style::INDEX.prefix(),
                idx,
                style::INDEX.suffix(),
                style::COMMENT.prefix(),
                pool::get_str(pool, *idx),
                style::COMMENT.suffix()
            );
        }

        Constant::MethodType(idx) => {
            pad_constant_name("MethodType", style::METHOD_TYPE);
            println!(
                "{}#{}{} {}// {}{}",
                style::INDEX.prefix(),
                idx,
                style::INDEX.suffix(),
                style::COMMENT.prefix(),
                pool::get_str(pool, *idx),
                style::COMMENT.suffix()
            );
        }

        Constant::FieldRef {
            class,
            name_and_type,
        } => {
            pad_constant_name("FieldRef", style::FIELD_REF);
            match (&pool[*class], &pool[*name_and_type]) {
                (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                    println!(
                        "{}#{} #{}{} {}// {}.{}:{}{}",
                        style::INDEX.prefix(),
                        class,
                        name_and_type,
                        style::INDEX.suffix(),
                        style::COMMENT.prefix(),
                        pool::get_str(pool, *class_idx),
                        pool::get_str(pool, *name),
                        pool::get_str(pool, *ty),
                        style::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid field ref>"),
            }
        }

        Constant::MethodRef {
            class,
            name_and_type,
        } => {
            pad_constant_name("MethodRef", style::METHOD_REF);
            match (&pool[*class], &pool[*name_and_type]) {
                (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                    println!(
                        "{}#{} #{}{} {}// {}.{}:{}{}",
                        style::INDEX.prefix(),
                        class,
                        name_and_type,
                        style::INDEX.suffix(),
                        style::COMMENT.prefix(),
                        pool::get_str(pool, *class_idx),
                        pool::get_str(pool, *name),
                        pool::get_str(pool, *ty),
                        style::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid method ref>"),
            }
        }

        Constant::InterfaceMethodRef {
            class,
            name_and_type,
        } => {
            pad_constant_name("InterfaceMethodRef", style::INTERFACE_METHOD_REF);
            match (&pool[*class], &pool[*name_and_type]) {
                (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                    println!(
                        "{}#{} #{}{} {}// {}.{}:{}{}",
                        style::INDEX.prefix(),
                        class,
                        name_and_type,
                        style::INDEX.suffix(),
                        style::COMMENT.prefix(),
                        pool::get_str(pool, *class_idx),
                        pool::get_str(pool, *name),
                        pool::get_str(pool, *ty),
                        style::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid interface method ref>"),
            }
        }

        Constant::NameAndType { name, ty } => {
            pad_constant_name("NameAndType", style::NAME_AND_TYPE);
            println!(
                "{}#{} #{}{} {}// {}:{}{}",
                style::INDEX.prefix(),
                name,
                ty,
                style::INDEX.suffix(),
                style::COMMENT.prefix(),
                pool::get_str(pool, *name),
                pool::get_str(pool, *ty),
                style::COMMENT.suffix(),
            );
        }

        Constant::MethodHandle { kind: _, index } => {
            pad_constant_name("MethodHandle", style::METHOD_HANDLE);
            match &pool[*index] {
                Constant::MethodRef {
                    class,
                    name_and_type,
                } => match (&pool[*class], &pool[*name_and_type]) {
                    (Constant::Class(class_idx), Constant::NameAndType { name, ty }) => {
                        println!(
                            "{ip}#{a}{is} ({ip}#{b} #{c}{is}) {cp}// {d}.{e}:{f}{cs}",
                            ip = style::INDEX.prefix(),
                            is = style::INDEX.suffix(),
                            cp = style::COMMENT.prefix(),
                            cs = style::COMMENT.suffix(),
                            a = class,
                            b = name_and_type,
                            c = name_and_type,
                            d = pool::get_str(pool, *class_idx),
                            e = pool::get_str(pool, *name),
                            f = pool::get_str(pool, *ty),
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
            pad_constant_name("InvokeDynamic", style::INVOKE_DYNAMIC);
            match &pool[*name_and_type] {
                Constant::NameAndType { name, ty } => {
                    println!(
                        "#{} {}#{}{} {}// {}:{}{}",
                        bootstrap_method_attr,
                        style::INDEX.prefix(),
                        name_and_type,
                        style::INDEX.suffix(),
                        style::COMMENT.prefix(),
                        pool::get_str(pool, *name),
                        pool::get_str(pool, *ty),
                        style::COMMENT.suffix(),
                    );
                }
                _ => println!("<invalid invoke dynamic>"),
            }
        }
    }
}
