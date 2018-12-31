use crate::code::{def::InstructionEntry, Range};
use ::class::{
    attribute::{Attribute, AttributeInfo, Code},
    class::{self, Class},
    field, method,
};
use ansi_term::Style;
use std::{fs::File, io::Read, path::PathBuf};
use structopt::StructOpt;

pub mod code;
pub mod constant;
pub mod pool;
pub mod style;

pub fn display_style<T: std::fmt::Display>(item: &T, prefix: &str, style: Style) {
    print!("{}{}{}{}", style.prefix(), prefix, item, style.suffix());
}

fn pad(count: usize) {
    for _ in 0..count {
        print!("  ");
    }
}

fn print_field_descriptor(descriptor: &field::Descriptor) {
    match &descriptor.ty {
        field::FieldType::Byte => print!("{}", style::PRIMITIVE_TYPE.paint("byte")),
        field::FieldType::Char => print!("{}", style::PRIMITIVE_TYPE.paint("char")),
        field::FieldType::Double => print!("{}", style::PRIMITIVE_TYPE.paint("double")),
        field::FieldType::Float => print!("{}", style::PRIMITIVE_TYPE.paint("float")),
        field::FieldType::Int => print!("{}", style::PRIMITIVE_TYPE.paint("int")),
        field::FieldType::Long => print!("{}", style::PRIMITIVE_TYPE.paint("long")),
        field::FieldType::Short => print!("{}", style::PRIMITIVE_TYPE.paint("short")),
        field::FieldType::Boolean => print!("{}", style::PRIMITIVE_TYPE.paint("boolean")),
        field::FieldType::Object(name) => {
            let mut iter = std::str::from_utf8(&name)
                .unwrap_or("<not utf8>")
                .split("/");

            print!("{}", style::CLASS_NAME.paint(iter.next().unwrap()));

            for item in iter {
                print!(".{}", style::CLASS_NAME.paint(item));
            }
        }
    }

    for _ in 0..descriptor.dimensions {
        print!("[]");
    }
}

fn print_method_descriptor(descriptor: &method::Descriptor, name: &str) {
    match &descriptor.ret {
        method::ReturnDescriptor::Void => print!("{}", style::PRIMITIVE_TYPE.paint("void")),
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
    print!(
        "{}// {}{}: ",
        style::COMMENT.prefix(),
        match &attr.attr {
            Attribute::Other(_) => "?",
            _ => " ",
        },
        pool::get_str(&class.pool, attr.name)
    );

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
    print!("{}", style::COMMENT.suffix());

    if add_newline {
        println!();
    }
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
        write("public", style::ACCESS_PUBLIC);
    }

    if access.is(class::Access::ENUM) {
        write("enum", style::ACCESS_ENUM);
    } else if access.is(class::Access::ANNOTATION) {
        write("@interface", style::ACCESS_ANNOTATION);
    } else if access.is(class::Access::INTERFACE) {
        write("interface", style::ACCESS_INTERFACE);
    } else {
        if access.is(class::Access::FINAL) {
            write("final", style::ACCESS_FINAL);
        } else if access.is(class::Access::ABSTRACT) {
            write("abstract", style::ACCESS_ABSTRACT);
        }

        write("class", style::ACCESS_CLASS);
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
        write("public", style::ACCESS_PUBLIC);
    } else if access.is(field::Access::PROTECTED) {
        write("protected", style::ACCESS_PROTECTED);
    } else if access.is(field::Access::PRIVATE) {
        write("private", style::ACCESS_PRIVATE);
    }

    if access.is(field::Access::STATIC) {
        write("static", style::ACCESS_STATIC);
    }

    if access.is(field::Access::FINAL) {
        write("final", style::ACCESS_FINAL);
    } else if access.is(field::Access::TRANSIENT) {
        write("transient", style::ACCESS_TRANSIENT);
    }

    if access.is(field::Access::VOLATILE) {
        write("volatile", style::ACCESS_VOLATILE);
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
        write("public", style::ACCESS_PUBLIC);
    } else if access.is(method::Access::PROTECTED) {
        write("protected", style::ACCESS_PROTECTED);
    } else if access.is(method::Access::PRIVATE) {
        write("private", style::ACCESS_PRIVATE);
    }

    if access.is(method::Access::ABSTRACT) {
        write("abstract", style::ACCESS_ABSTRACT);
    } else {
        if access.is(method::Access::STATIC) {
            write("static", style::ACCESS_STATIC);
        }

        if access.is(method::Access::FINAL) {
            write("final", style::ACCESS_FINAL);
        }

        if access.is(method::Access::SYNCHRONIZED) {
            write("synchronized", style::ACCESS_SYNCHRONIZED);
        }

        if access.is(method::Access::STRICT) {
            write("strictfp", style::ACCESS_STRICTFP);
        }

        if access.is(method::Access::NATIVE) {
            write("native", style::ACCESS_NATIVE);
        }
    }

    if was_written {
        print!(" ");
    }
}

fn print_class_decl(class: &Class) {
    print_class_access(&class.access_flags);
    print!(
        " {}",
        style::CLASS_NAME.paint(pool::get_class_name(&class.pool, class.this_class))
    );

    match pool::get_class_name(&class.pool, class.super_class) {
        "java/lang/Object" | "java/lang/Enum" => {}
        _ => print!(
            " {} {}",
            style::EXTENDS.paint("extends"),
            style::CLASS_NAME.paint(pool::get_class_name(&class.pool, class.super_class))
        ),
    }

    if class.interfaces.len() > 0 {
        print!(" {} ", style::EXTENDS.paint("implements"));
        // pprint_list(class.interfaces.iter().map(|idx|
        // pool::get_class_name(&class.pool, idx)), style::CLASS_NAME);

        let len = class.interfaces.len();
        for &idx in &class.interfaces[..len - 1] {
            print!(
                "{}{}{}, ",
                style::CLASS_NAME.prefix(),
                pool::get_class_name(&class.pool, idx),
                style::CLASS_NAME.suffix()
            );
        }

        print!(
            "{}{}{}",
            style::CLASS_NAME.prefix(),
            pool::get_class_name(&class.pool, class.interfaces[len - 1]),
            style::CLASS_NAME.suffix()
        );
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
                    let mut style = match (branch, forward) {
                        (false, false) => style::BACKWARD_JUMP,
                        (false, true) => style::FORWARD_JUMP,
                        (true, false) => style::BACKWARD_BRANCH,
                        (true, true) => style::FORWARD_BRANCH,
                    };

                    if slot % 2 == 1 {
                        style = style.dimmed();
                    }

                    print!("{}", style.paint(v));
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

        use crate::code::{def::ArrayPrimitiveType, InstructionCategory};

        let style = match code::get_category(instruction) {
            InstructionCategory::LoadAndStore => style::OPCODE_LOAD_STORE,
            InstructionCategory::OperandStackManagement => style::OPCODE_OPERAND_STACK_MANAGEMENT,
            InstructionCategory::ArithmeticAndLogic => style::OPCODE_ARITH_LOGIC,
            InstructionCategory::TypeConversion => style::OPCODE_TYPE_CONVERSION,
            InstructionCategory::ControlFlow => style::OPCODE_CONTROL_FLOW,
            InstructionCategory::InvocationAndReturn => style::OPCODE_INVOCATION_RETURN,
            InstructionCategory::ObjectManipulation => style::OPCODE_OBJECT_MANIPULATION,
            InstructionCategory::Other => style::OPCODE_DEFAULT,
        };

        // TODO: spacing for byte positions
        // TODO: code after method?

        print!("{} ", style::COMMENT.paint("//"),);

        print_jmps(start);

        print!(
            " {}/{}: {} ",
            start,
            index,
            style.paint(code::instruction_name(tag))
        );

        match instruction {
            Instruction::ConstInt(k) => display_style(k, "$", style::OPCODE_IMMEDIATE_OTHER),
            Instruction::ConstLong(k) => display_style(k, "$", style::OPCODE_IMMEDIATE_OTHER),
            Instruction::ConstFloat(k) => display_style(k, "$", style::OPCODE_IMMEDIATE_OTHER),
            Instruction::ConstDouble(k) => display_style(k, "$", style::OPCODE_IMMEDIATE_OTHER),

            Instruction::PushByte(k) => display_style(k, "$", style::OPCODE_IMMEDIATE_OTHER),
            Instruction::PushShort(k) => display_style(k, "$", style::OPCODE_IMMEDIATE_OTHER),

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
                display_style(idx, "#", style::OPCODE_IMMEDIATE_INDEX)
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
            | Instruction::StoreRef(idx) => display_style(idx, "#", style::OPCODE_IMMEDIATE_INDEX),

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
                display_style(offset, "@", style::OPCODE_IMMEDIATE_BRANCH)
            }

            Instruction::Goto(offset) | Instruction::Jsr(offset) => {
                display_style(offset, "@", style::OPCODE_IMMEDIATE_BRANCH)
            }
            Instruction::Ret(idx) => display_style(idx, "@", style::OPCODE_IMMEDIATE_BRANCH),

            Instruction::InvokeInterface(idx, len) | Instruction::NewArrayMultiRef(idx, len) => {
                display_style(idx, "#", style::OPCODE_IMMEDIATE_INDEX);
                display_style(len, "^", style::OPCODE_IMMEDIATE_INDEX);
            }

            Instruction::NewArrayPrimitive(ty) => match ty {
                ArrayPrimitiveType::Boolean => {
                    display_style(&"boolean", "", style::OPCODE_IMMEDIATE_OTHER)
                }
                ArrayPrimitiveType::Char => {
                    display_style(&"char", "", style::OPCODE_IMMEDIATE_OTHER)
                }
                ArrayPrimitiveType::Float => {
                    display_style(&"float", "", style::OPCODE_IMMEDIATE_OTHER)
                }
                ArrayPrimitiveType::Double => {
                    display_style(&"double", "", style::OPCODE_IMMEDIATE_OTHER)
                }
                ArrayPrimitiveType::Byte => {
                    display_style(&"byte", "", style::OPCODE_IMMEDIATE_OTHER)
                }
                ArrayPrimitiveType::Short => {
                    display_style(&"short", "", style::OPCODE_IMMEDIATE_OTHER)
                }
                ArrayPrimitiveType::Int => display_style(&"int", "", style::OPCODE_IMMEDIATE_OTHER),
                ArrayPrimitiveType::Long => {
                    display_style(&"long", "", style::OPCODE_IMMEDIATE_OTHER)
                }
            },

            _ => (),
        }
        println!();
    }

    use petgraph::visit::*;

    let cfg = code::create_control_flow_graph(&instructions);

    println!("digraph {{");

    for node in cfg.node_references() {
        let id = cfg.to_index(node.id());
        let (_, range) = node.weight();

        pad(1);
        print!("{} [shape=\"rect\" label=\"", id);
        for instruction in &instructions[range.start..range.end] {
            print!("{}\\n", code::instruction_name(instruction.tag));
        }
        print!(
            "{}",
            code::INSTRUCTION_NAMES[instructions[range.end].tag as usize]
        );
        println!("\"]");
    }

    for edge in cfg.edge_references() {
        pad(1);
        print!(
            "{} -> {}",
            cfg.to_index(edge.source()),
            cfg.to_index(edge.target())
        );

        if let Some(branch) = edge.weight() {
            match branch {
                true => print!(" [label=\"T\"]"),
                false => print!(" [label=\"F\"]"),
            }
        }

        println!();
    }

    println!("}}");
}

#[derive(Clone, Debug, PartialEq, StructOpt)]
#[structopt(name = "ppclass")]
struct App {
    /// Input files to parse.
    #[structopt(name = "FILE", parse(from_os_str))]
    input: Vec<PathBuf>,

    /// Turn off color output.
    #[structopt(long = "no-color")]
    no_color: bool,

    /// Print the constant pool.
    #[structopt(short = "c", long = "constant-pool")]
    show_constant_pool: bool,

    /// Print the class declaration, fields, and methods.
    #[structopt(short = "d", long = "declarations")]
    show_decl: bool,
}

fn parse_class(app: &App, buf: &[u8]) {
    let class = Class::parse(buf).unwrap();

    if app.show_constant_pool {
        for entry in 0..class.pool.len() {
            print!(
                "{}{:5}{} = ",
                style::INDEX.prefix(),
                entry,
                style::INDEX.suffix()
            );
            constant::print_constant(&class.pool, entry, 0);
        }
    }

    if app.show_decl {
        for attr in &*class.attributes {
            print_attribute(&class, &attr, 0);
        }
        println!(
            "{}// Version {}.{}{}",
            style::COMMENT.prefix(),
            class.version.major,
            class.version.minor,
            style::COMMENT.suffix()
        );

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
    let app = App::from_args();

    let mut buf = Vec::new();
    for path in &app.input {
        let mut file = File::open(path).unwrap();
        file.read_to_end(&mut buf).unwrap();

        parse_class(&app, &buf);

        buf.clear();
    }
}
