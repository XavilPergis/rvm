use crate::{
    code::{def::InstructionEntry, Range},
    style::STYLE_MAP,
};
use ::class::{
    attribute::{Attribute, AttributeInfo, Code},
    class::{self, Class},
    field, method,
};
use std::{fs::File, io::Read, path::PathBuf};
use structopt::StructOpt;

pub mod code;
pub mod constant;
pub mod pool;
pub mod style;

fn pad(count: usize) {
    for _ in 0..count {
        print!("  ");
    }
}

fn print_field_descriptor(descriptor: &field::Descriptor) {
    match &descriptor.ty {
        field::FieldType::Byte => APP.paint("type.primitive.byte", || print!("byte")),
        field::FieldType::Char => APP.paint("type.primitive.char", || print!("char")),
        field::FieldType::Double => APP.paint("type.primitive.double", || print!("double")),
        field::FieldType::Float => APP.paint("type.primitive.float", || print!("float")),
        field::FieldType::Int => APP.paint("type.primitive.int", || print!("int")),
        field::FieldType::Long => APP.paint("type.primitive.long", || print!("long")),
        field::FieldType::Short => APP.paint("type.primitive.short", || print!("short")),
        field::FieldType::Boolean => APP.paint("type.primitive.boolean", || print!("boolean")),
        field::FieldType::Object(name) => {
            let mut iter = std::str::from_utf8(&name)
                .unwrap_or("<not utf8>")
                .split("/");

            APP.paint("type.object", || print!("{}", iter.next().unwrap()));

            for item in iter {
                APP.paint("type.object", || print!(".{}", item));
            }
        }
    }

    for _ in 0..descriptor.dimensions {
        print!("[]");
    }
}

fn print_method_descriptor(descriptor: &method::Descriptor, name: &str) {
    match &descriptor.ret {
        method::ReturnDescriptor::Void => APP.paint("type.primitive.void", || print!("void")),
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

fn print_attribute(class: &Class, attr: &AttributeInfo, depth: usize) {
    let mut add_newline = true;
    pad(depth);

    APP.paint("comment", || {
        print!(
            "// {}{}: ",
            match &attr.attr {
                Attribute::Other(_) => "?",
                _ => " ",
            },
            pool::get_str(&class.pool, attr.name)
        );
    });

    match &attr.attr {
        Attribute::Other(bytes) => {
            for b in &**bytes {
                print!("{:02x} ", b);
            }
        }

        Attribute::Code(code) => {
            println!();
            print_code(code);
            add_newline = false;
            // for b in &*code.code {
            //     print!("{:02x} ", b);
            // }
        }

        Attribute::StackMapTable(_) => print!("StackMapTable"),
        Attribute::ConstantValue(idx) => print!("{:?}", &class.pool[*idx]),
        Attribute::Signature(idx) => print!("{}", pool::get_str(&class.pool, *idx)),
    }

    if add_newline {
        println!();
    }
}

fn print_class_access(access: &class::Access) {
    let mut was_written = false;
    let mut write = |s, col| {
        if was_written {
            APP.paint(col, || print!(" {}", s));
        } else {
            APP.paint(col, || print!("{}", s));
            was_written = true;
        }
    };

    if access.is(class::Access::PUBLIC) {
        write("public", "access.visibility.public");
    }

    if access.is(class::Access::ENUM) {
        write("enum", "access.class.enum");
    } else if access.is(class::Access::ANNOTATION) {
        write("@interface", "access.class.annotation");
    } else if access.is(class::Access::INTERFACE) {
        write("interface", "access.class.interface");
    } else {
        if access.is(class::Access::FINAL) {
            write("final", "access.other.final");
        } else if access.is(class::Access::ABSTRACT) {
            write("abstract", "access.other.abstract");
        }

        write("class", "access.class.class");
    }
}

fn print_field_access(access: &field::Access) {
    let mut was_written = false;
    let mut write = |s, col| {
        if was_written {
            APP.paint(col, || print!(" {}", s));
        } else {
            APP.paint(col, || print!("{}", s));
            was_written = true;
        }
    };

    if access.is(field::Access::PUBLIC) {
        write("public", "access.visibility.public");
    } else if access.is(field::Access::PROTECTED) {
        write("protected", "access.visibility.protected");
    } else if access.is(field::Access::PRIVATE) {
        write("private", "access.visibility.private");
    }

    if access.is(field::Access::STATIC) {
        write("static", "access.other.static");
    }

    if access.is(field::Access::FINAL) {
        write("final", "access.other.final");
    } else if access.is(field::Access::TRANSIENT) {
        write("transient", "access.field.transient");
    }

    if access.is(field::Access::VOLATILE) {
        write("volatile", "access.field.volatile");
    }

    if was_written {
        print!(" ");
    }
}

fn print_method_access(access: &method::Access) {
    let mut was_written = false;
    let mut write = |s, col| {
        if was_written {
            APP.paint(col, || print!(" {}", s));
        } else {
            APP.paint(col, || print!("{}", s));
            was_written = true;
        }
    };

    if access.is(method::Access::PUBLIC) {
        write("public", "access.visibility.public");
    } else if access.is(method::Access::PROTECTED) {
        write("protected", "access.visibility.protected");
    } else if access.is(method::Access::PRIVATE) {
        write("private", "access.visibility.private");
    }

    if access.is(method::Access::ABSTRACT) {
        write("abstract", "access.other.static");
    } else {
        if access.is(method::Access::STATIC) {
            write("static", "access.other.static");
        }

        if access.is(method::Access::FINAL) {
            write("final", "access.other.final");
        }

        if access.is(method::Access::SYNCHRONIZED) {
            write("synchronized", "access.method.synchronized");
        }

        if access.is(method::Access::STRICT) {
            write("strictfp", "access.method.strictfp");
        }

        if access.is(method::Access::NATIVE) {
            write("native", "access.method.native");
        }
    }

    if was_written {
        print!(" ");
    }
}

fn print_class_decl(class: &Class) {
    print_class_access(&class.access_flags);

    APP.paint("type.object", || {
        print!(" {}", pool::get_class_name(&class.pool, class.this_class))
    });

    match pool::get_class_name(&class.pool, class.super_class) {
        "java/lang/Object" | "java/lang/Enum" => {}
        _ => {
            APP.paint("extends", || print!(" extends "));
            APP.paint("type.object", || {
                print!("{}", pool::get_class_name(&class.pool, class.super_class))
            });
        }
    }

    if class.interfaces.len() > 0 {
        APP.paint("extends", || print!(" implements "));

        let len = class.interfaces.len();
        for &idx in &class.interfaces[..len - 1] {
            APP.paint("type.object", || {
                print!("{}", pool::get_class_name(&class.pool, idx))
            });
            print!(", ");
        }

        APP.paint("type.object", || {
            print!(
                "{}",
                pool::get_class_name(&class.pool, class.interfaces[len - 1])
            )
        });
    }
}

fn print_code(code: &Code) {
    use crate::code::Instruction;

    let mut instructions = Vec::with_capacity(code.code.len());
    code::parse_instructions_into(&code.code, |entry| {
        instructions.push(entry);
    });

    use std::collections::HashSet;

    // list of intervals covering [source_addr, target_addr]
    let mut intervals = Vec::new();
    let mut branches: HashSet<Range> = HashSet::new();
    for &InstructionEntry {
        start,
        ref instruction,
        ..
    } in &instructions
    {
        let istart = start as isize;
        match instruction {
            Instruction::IfEqual(off)
            | Instruction::IfNotEqual(off)
            | Instruction::IfLessThan(off)
            | Instruction::IfGreaterThan(off)
            | Instruction::IfLessThanEqual(off)
            | Instruction::IfGreaterThanEqual(off)
            | Instruction::IfEqualInt(off)
            | Instruction::IfNotEqualInt(off)
            | Instruction::IfLessThanInt(off)
            | Instruction::IfGreaterThanEqualInt(off)
            | Instruction::IfGreaterThanInt(off)
            | Instruction::IfLessThanEqualInt(off)
            | Instruction::IfEqualRef(off)
            | Instruction::IfNotEqualRef(off)
            | Instruction::IfNull(off)
            | Instruction::IfNonNull(off) => {
                let src = start;
                let dest = (istart + *off as isize) as usize;
                intervals.push((src..dest).into());
                branches.insert((src..dest).into());
            }

            Instruction::Goto(off) => {
                let src = start;
                let dest = (istart + *off as isize) as usize;
                intervals.push((src..dest).into());
            }

            _ => (),
        }
    }

    let layers = code::partiton_jumps(&intervals);

    let print_jmps = |idx| {
        for slot in (0..layers.len()).rev() {
            let mut was_printed = false;

            let mut print = |v, branch, forward| {
                if !was_printed {
                    let style = match (branch, forward) {
                        (false, false) => "flow.jump.backward",
                        (false, true) => "flow.jump.forward",
                        (true, false) => "flow.branch.backward",
                        (true, true) => "flow.branch.forward",
                    };

                    // TODO: clean this up somehow?
                    if APP.no_color {
                        print!("{}", v);
                    } else {
                        let style = if slot % 2 == 1 {
                            STYLE_MAP[style].dimmed()
                        } else {
                            STYLE_MAP[style]
                        };
                        print!("{}", style.paint(v));
                    }

                    was_printed = true;
                }
            };

            let layer = &layers[slot];
            for range in layer {
                if idx == range.start {
                    if range.start <= range.end {
                        print("┍", branches.contains(range), true);
                    } else {
                        print("┕", branches.contains(range), false);
                    }
                }
                if idx == range.end {
                    if range.start <= range.end {
                        print("┕", branches.contains(range), true);
                    } else {
                        print("┍", branches.contains(range), false);
                    }
                }
                if range.contains(idx) {
                    print("│", branches.contains(range), range.start <= range.end);
                }
            }

            // Fill the column with *something*
            if !was_printed {
                print!(" ");
            }
        }
    };

    for (
        index,
        &InstructionEntry {
            start,
            tag,
            ref instruction,
        },
    ) in instructions.iter().enumerate()
    {
        pad(1);

        use crate::code::def::ArrayPrimitiveType;

        // TODO: spacing for byte positions
        // TODO: code after method?

        APP.paint("comment", || print!("//"));

        print_jmps(start);

        print!(" {}/{}: ", start, index,);

        APP.paint(code::get_style_key(instruction), || {
            print!("{}", code::instruction_name(tag));
        });

        match instruction {
            Instruction::ConstInt(k) => {
                print!(" $");
                APP.paint("opcode.immediate.other", || print!("{}", k));
            }
            Instruction::ConstLong(k) => {
                print!(" $");
                APP.paint("opcode.immediate.other", || print!("{}", k));
            }
            Instruction::ConstFloat(k) => {
                print!(" $");
                APP.paint("opcode.immediate.other", || print!("{}", k));
            }
            Instruction::ConstDouble(k) => {
                print!(" $");
                APP.paint("opcode.immediate.other", || print!("{}", k));
            }

            Instruction::PushByte(k) => {
                print!(" $");
                APP.paint("opcode.immediate.other", || print!("{}", k));
            }
            Instruction::PushShort(k) => {
                print!(" $");
                APP.paint("opcode.immediate.other", || print!("{}", k));
            }

            Instruction::InvokeVirtual(idx)
            | Instruction::InvokeSpecial(idx)
            | Instruction::InvokeStatic(idx)
            | Instruction::InvokeDynamic(idx)
            | Instruction::New(idx)
            | Instruction::NewArrayRef(idx)
            | Instruction::CheckCast(idx)
            | Instruction::InstanceOf(idx)
            | Instruction::GetStatic(idx)
            | Instruction::PutStatic(idx)
            | Instruction::GetField(idx)
            | Instruction::PutField(idx)
            | Instruction::LoadConstant(idx) => {
                print!(" #");
                APP.paint("opcode.immediate.index", || print!("{}", idx));
            }

            Instruction::LoadInt(idx)
            | Instruction::LoadLong(idx)
            | Instruction::LoadFloat(idx)
            | Instruction::LoadDouble(idx)
            | Instruction::LoadRef(idx)
            | Instruction::StoreInt(idx)
            | Instruction::StoreLong(idx)
            | Instruction::StoreFloat(idx)
            | Instruction::StoreDouble(idx)
            | Instruction::StoreRef(idx) => {
                print!(" #");
                APP.paint("opcode.immediate.index", || print!("{}", idx));
            }

            Instruction::IfEqual(offset)
            | Instruction::IfNotEqual(offset)
            | Instruction::IfLessThan(offset)
            | Instruction::IfGreaterThan(offset)
            | Instruction::IfLessThanEqual(offset)
            | Instruction::IfGreaterThanEqual(offset)
            | Instruction::IfEqualInt(offset)
            | Instruction::IfNotEqualInt(offset)
            | Instruction::IfLessThanInt(offset)
            | Instruction::IfGreaterThanEqualInt(offset)
            | Instruction::IfGreaterThanInt(offset)
            | Instruction::IfLessThanEqualInt(offset)
            | Instruction::IfEqualRef(offset)
            | Instruction::IfNotEqualRef(offset)
            | Instruction::IfNull(offset)
            | Instruction::IfNonNull(offset) => {
                print!(" @");
                APP.paint("opcode.immediate.branch", || print!("{}", offset));
            }

            Instruction::Goto(offset) | Instruction::Jsr(offset) => {
                print!(" @");
                APP.paint("opcode.immediate.branch", || print!("{}", offset));
            }
            Instruction::Ret(idx) => {
                print!(" #");
                APP.paint("opcode.immediate.branch", || print!("{}", idx));
            }

            Instruction::InvokeInterface(idx, len) | Instruction::NewArrayMultiRef(idx, len) => {
                print!(" #");
                APP.paint("opcode.immediate.index", || print!("{}", idx));
                print!(" ^");
                APP.paint("opcode.immediate.other", || print!("{}", len));
            }

            Instruction::NewArrayPrimitive(ty) => APP.paint("opcode.immediate.other", || {
                print!(
                    " {}",
                    match ty {
                        ArrayPrimitiveType::Boolean => "boolean",
                        ArrayPrimitiveType::Char => "char",
                        ArrayPrimitiveType::Float => "float",
                        ArrayPrimitiveType::Double => "double",
                        ArrayPrimitiveType::Byte => "byte",
                        ArrayPrimitiveType::Short => "short",
                        ArrayPrimitiveType::Int => "int",
                        ArrayPrimitiveType::Long => "long",
                    }
                )
            }),

            _ => (),
        }
        println!();
    }

    // use petgraph::visit::*;

    // let cfg = code::create_control_flow_graph(&instructions);

    // println!("digraph {{");

    // for node in cfg.node_references() {
    //     let id = cfg.to_index(node.id());
    //     let (_, range) = node.weight();

    //     pad(1);
    //     print!("{} [shape=\"rect\" label=\"", id);
    //     for instruction in &instructions[range.start..range.end] {
    //         print!("{}\\n", code::instruction_name(instruction.tag));
    //     }
    //     print!(
    //         "{}",
    //         code::INSTRUCTION_NAMES[instructions[range.end].tag as usize]
    //     );
    //     println!("\"]");
    // }

    // for edge in cfg.edge_references() {
    //     pad(1);
    //     print!(
    //         "{} -> {}",
    //         cfg.to_index(edge.source()),
    //         cfg.to_index(edge.target())
    //     );

    //     if let Some(branch) = edge.weight() {
    //         match branch {
    //             true => print!(" [label=\"T\"]"),
    //             false => print!(" [label=\"F\"]"),
    //         }
    //     }

    //     println!();
    // }

    // println!("}}");
}

lazy_static::lazy_static! {
    pub static ref APP: App = App::from_args();
}

#[derive(Clone, Debug, PartialEq, StructOpt)]
#[structopt(name = "ppclass")]
pub struct App {
    /// Input files to parse.
    #[structopt(name = "FILE", parse(from_os_str))]
    pub input: Vec<PathBuf>,

    /// Turn off color output.
    #[structopt(long = "no-color")]
    pub no_color: bool,

    /// Print the constant pool.
    #[structopt(short = "c", long = "constant-pool")]
    pub show_constant_pool: bool,

    /// Print the class declaration, fields, and methods.
    #[structopt(short = "d", long = "declarations")]
    pub show_decl: bool,

    /// Print the `Code` attribute for each method.
    #[structopt(short = "C", long = "code")]
    pub show_code: bool,
}

impl App {
    pub fn paint<F>(&self, style: &str, func: F)
    where
        F: FnOnce(),
    {
        if self.no_color {
            func();
        } else {
            print!("{}", STYLE_MAP[style].prefix());
            func();
            print!("{}", STYLE_MAP[style].suffix());
        }
    }
}

fn parse_class(buf: &[u8]) {
    let class = Class::parse(buf).unwrap();

    if APP.show_constant_pool {
        for entry in 1..class.pool.len() {
            APP.paint("pool.index", || print!("{:5}", entry));
            print!(" = ");
            constant::print_constant(&class.pool, entry, 0);
        }
    }

    if APP.show_decl {
        for attr in &*class.attributes {
            print_attribute(&class, &attr, 0);
        }

        APP.paint("comment", || {
            println!("// Version {}.{}", class.version.major, class.version.minor);
        });

        print_class_decl(&class);

        println!(" {{");

        for field in &*class.fields {
            for attr in &*field.attributes {
                print_attribute(&class, &attr, 1);
            }
            pad(1);
            print_field_access(&field.access);
            let name = pool::get_str(&class.pool, field.name);

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
            let name = pool::get_str(&class.pool, method.name);

            print_method_descriptor(&method.descriptor, name);
            println!(";");
        }

        println!("}}");
    }
}

fn main() {
    let mut buf = Vec::new();
    for path in &APP.input {
        let mut file = File::open(path).unwrap();
        file.read_to_end(&mut buf).unwrap();

        parse_class(&buf);

        buf.clear();
    }
}
