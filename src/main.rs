use rvm::raw::{class::Class, constant::Constant};
use std::{fs::File, io::Read};

fn main() {
    let mut buf = Vec::new();
    let mut file = File::open(std::env::args().skip(1).next().unwrap()).unwrap();
    file.read_to_end(&mut buf).unwrap();

    let class = Class::parse(buf).unwrap();

    fn print_constant(pool: &[Constant], index: usize, depth: usize) {
        for _ in 0..depth {
            print!("    ");
        }

        match &pool[index] {
            Constant::Integer(val) => println!("int: {}", val),
            Constant::Float(val) => println!("float: {}", val),
            Constant::Long(val) => println!("long: {}", val),
            Constant::Double(val) => println!("double: {}", val),

            Constant::StringData(data) => println!(
                "string data: {}",
                std::str::from_utf8(data).unwrap_or("<NOT UTF8>")
            ),

            Constant::String(idx) => {
                println!("string: {}", idx);
                print_constant(pool, *idx, depth + 1);
            }

            Constant::Class(idx) => {
                println!("class: {}", idx);
                print_constant(pool, *idx, depth + 1);
            }

            Constant::MethodType(idx) => {
                println!("method type: {}", idx);
                print_constant(pool, *idx, depth + 1);
            }

            Constant::FieldRef {
                class,
                name_and_type,
            } => {
                println!("field: {} {}", class, name_and_type);
                print_constant(pool, *class, depth + 1);
                print_constant(pool, *name_and_type, depth + 1);
            }

            Constant::MethodRef {
                class,
                name_and_type,
            } => {
                println!("method: {} {}", class, name_and_type);
                print_constant(pool, *class, depth + 1);
                print_constant(pool, *name_and_type, depth + 1);
            }

            Constant::InterfaceMethodRef {
                class,
                name_and_type,
            } => {
                println!("interface method: {} {}", class, name_and_type);
                print_constant(pool, *class, depth + 1);
                print_constant(pool, *name_and_type, depth + 1);
            }

            Constant::NameAndType { name, ty } => {
                println!("name and type: {} {}", name, ty);
                print_constant(pool, *name, depth + 1);
                print_constant(pool, *ty, depth + 1);
            }

            Constant::MethodHandle { kind, index } => {
                println!("method handle: {:?} {}", kind, index);
                print_constant(pool, *index, depth + 1);
            }

            Constant::InvokeDynamic {
                bootstrap_method_attr,
                name_and_type,
            } => {
                println!(
                    "invoke dynamic: {} {}",
                    bootstrap_method_attr, name_and_type
                );
                print_constant(pool, *bootstrap_method_attr, depth + 1);
                print_constant(pool, *name_and_type, depth + 1);
            }

            _ => (),
        }
    }

    for entry in 0..class.constant_pool.len() {
        print!("#{} -> ", entry);
        print_constant(&class.constant_pool, entry, 0);
    }

    let get_class_name = |idx| match &class.constant_pool[idx] {
        Constant::Class(idx) => match &class.constant_pool[*idx] {
            Constant::StringData(data) => std::str::from_utf8(data).unwrap_or("<NOT UTF8>"),
            _ => "<SYMBOL NOT FOUND>",
        },
        _ => "<SYMBOL NOT FOUND>",
    };

    println!("Version {}.{}", class.version.major, class.version.minor);
    // println!("Access Flags: {}", class.access_flags);
    println!("This Class: {}", class.this_class);
    print_constant(&class.constant_pool, class.this_class, 1);
    println!("Super Class: {}", class.super_class);
    print_constant(&class.constant_pool, class.super_class, 1);

    print!(
        "{}{} ",
        class.access_flags,
        get_class_name(class.this_class)
    );

    match get_class_name(class.super_class) {
        "java/lang/Object" | "java/lang/Enum" => {}
        _ => print!("extends {}", get_class_name(class.super_class)),
    }

    println!();
}
