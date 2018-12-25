use rvm::raw::{class::Class, constant::Constant};
use std::{collections::HashSet, fs::File, io::Read};

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

    fn print_constant(pool: &[Constant], index: usize, depth: usize) {
        for _ in 0..depth {
            print!("    ");
        }

        match &pool[index] {
            Constant::Nothing => println!("<nothing>"),
            Constant::Integer(val) => println!("int: {}", val),
            Constant::Float(val) => println!("float: {}", val),
            Constant::Long(val) => println!("long: {}", val),
            Constant::Double(val) => println!("double: {}", val),

            Constant::StringData(data) => println!(
                "string data: {}",
                std::str::from_utf8(data).unwrap_or("<not utf8>")
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
        }
    }

    if short_set.contains(&'c') || long_set.contains("constant-pool") {
        for entry in 0..class.constant_pool.len() {
            print!("#{} -> ", entry);
            print_constant(&class.constant_pool, entry, 0);
        }
    }

    let get_class_name = |idx| match &class.constant_pool[idx] {
        Constant::Class(idx) => match &class.constant_pool[*idx] {
            Constant::StringData(data) => std::str::from_utf8(data).unwrap_or("<not utf8>"),
            _ => "<symbol not found>",
        },
        _ => "<symbol not found>",
    };

    println!("Version {}.{}", class.version.major, class.version.minor);

    print!(
        "{} {} ",
        class.access_flags,
        get_class_name(class.this_class)
    );

    match get_class_name(class.super_class) {
        "java/lang/Object" | "java/lang/Enum" => {}
        _ => print!("extends {} ", get_class_name(class.super_class)),
    }

    if class.interfaces.len() > 0 {
        print!("implements ");
        for &iface in &class.interfaces[..class.interfaces.len() - 1] {
            print!("{}, ", get_class_name(iface));
        }
        print!(
            "{}",
            get_class_name(class.interfaces[class.interfaces.len() - 1])
        );
    }

    println!(" {{");
    for field in &*class.fields {
        print!("    {} ", field.access);
        let name = std::str::from_utf8(
            class.constant_pool[field.name]
                .as_string_data()
                .unwrap_or(b"<bad field name>"),
        )
        .unwrap_or("<not utf8>");

        println!("{} {};", field.descriptor, name);
    }

    println!();

    for method in &*class.methods {
        print!("    {} ", method.access);
        let name = std::str::from_utf8(
            class.constant_pool[method.name]
                .as_string_data()
                .unwrap_or(b"<bad field name>"),
        )
        .unwrap_or("<not utf8>");

        print!("{} {}(", method.descriptor.ret, name);
        pprint_list(&method.descriptor.args);
        println!(");");
    }
    println!("}}");
    // println!("{:?}", class);
}

fn pprint_list<T: std::fmt::Display>(items: &[T]) {
    if items.len() > 0 {
        for item in &items[..items.len() - 1] {
            print!("{}, ", item);
        }

        print!("{}", &items[items.len() - 1]);
    }
}
