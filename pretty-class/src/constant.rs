use crate::{pad, APP};
use class::constant::Constant;

fn print_contant_type(pool: &[Constant], index: usize) {
    fn pad_constant_name(name: &str, style: &str) {
        APP.paint(style, || print!("{}", name));
        for _ in 0..("InterfaceMethodRef".len() - name.len()) {
            print!(" ");
        }
    }

    match &pool[index] {
        Constant::Nothing => (),
        Constant::Integer(_) => pad_constant_name("Int", "pool.val.int"),
        Constant::Float(_) => pad_constant_name("Float", "pool.val.float"),
        Constant::Long(_) => pad_constant_name("Long", "pool.val.long"),
        Constant::Double(_) => pad_constant_name("Double", "pool.val.double"),
        Constant::StringData(_) => pad_constant_name("String", "pool.val.string"),
        Constant::String(_) => pad_constant_name("StringRef", "pool.ref.string"),
        Constant::Class(_) => pad_constant_name("ClassRef", "pool.ref.class"),
        Constant::MethodType(_) => pad_constant_name("MethodType", "pool.ref.method_type"),
        Constant::FieldRef { .. } => pad_constant_name("FieldRef", "pool.ref.member.field"),
        Constant::MethodRef { .. } => pad_constant_name("MethodRef", "pool.ref.member.method"),
        Constant::InterfaceMethodRef { .. } => {
            pad_constant_name("InterfaceMethodRef", "pool.ref.member.interface_method")
        }
        Constant::NameAndType { .. } => pad_constant_name("NameAndType", "pool.ref.name_and_type"),
        Constant::MethodHandle { .. } => {
            pad_constant_name("MethodHandle", "pool.ref.method_handle")
        }
        Constant::InvokeDynamic { .. } => {
            pad_constant_name("InvokeDynamic", "pool.ref.invoke_dynamic")
        }
    }
}

fn print_constant_index(pool: &[Constant], index: usize) -> bool {
    match &pool[index] {
        Constant::InvokeDynamic {
            name_and_type: idx, ..
        }
        | Constant::MethodHandle { index: idx, .. }
        | Constant::String(idx)
        | Constant::Class(idx)
        | Constant::MethodType(idx) => APP.paint("pool.index", || print!("#{}", idx)),

        Constant::FieldRef {
            class,
            name_and_type,
        }
        | Constant::MethodRef {
            class,
            name_and_type,
        }
        | Constant::InterfaceMethodRef {
            class,
            name_and_type,
        } => {
            APP.paint("pool.index", || print!("#{}", class));
            print!(".");
            print_constant_index(pool, *name_and_type);
        }

        Constant::NameAndType { name, ty } => {
            APP.paint("pool.index", || print!("#{}", name));
            print!(":");
            APP.paint("pool.index", || print!("#{}", ty));
        }

        // Everything else is a value; not a pool reference
        _ => return false,
    }

    true
}

fn print_constant_value(pool: &[Constant], index: usize) {
    match &pool[index] {
        Constant::Nothing => print!("<nothing>"),
        Constant::Integer(val) => APP.paint("pool.val.int", || print!("{}", val)),
        Constant::Float(val) => APP.paint("pool.val.float", || print!("{}", val)),
        Constant::Long(val) => APP.paint("pool.val.long", || print!("{}", val)),
        Constant::Double(val) => APP.paint("pool.val.double", || print!("{}", val)),

        Constant::StringData(data) => APP.paint("pool.val.string", || {
            print!("{}", std::str::from_utf8(data).unwrap_or("<not utf8>"))
        }),

        Constant::InvokeDynamic {
            name_and_type: idx, ..
        }
        | Constant::MethodHandle { index: idx, .. }
        | Constant::String(idx)
        | Constant::Class(idx)
        | Constant::MethodType(idx) => {
            print_constant_value(pool, *idx);
        }

        Constant::FieldRef {
            class,
            name_and_type,
        }
        | Constant::MethodRef {
            class,
            name_and_type,
        }
        | Constant::InterfaceMethodRef {
            class,
            name_and_type,
        } => {
            print_constant_value(pool, *class);
            print_constant_value(pool, *name_and_type);
        }

        Constant::NameAndType { name, ty } => {
            print_constant_value(pool, *name);
            print!(":");
            print_constant_value(pool, *ty);
        }
    }
}

pub fn print_constant(pool: &[Constant], index: usize, depth: usize) {
    pad(depth);
    print_contant_type(pool, index);
    if print_constant_index(pool, index) {
        print!(" ");
    }
    print_constant_value(pool, index);
    println!();
}
