use ::class::{
    attribute::{Attribute, AttributeInfo, Code},
    class::{self, Class},
    constant::Constant,
    field, method,
};
use ansi_term::Style;
use std::{fs::File, io::Read, path::PathBuf};
use structopt::StructOpt;

pub mod code;
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    fn inverted(self) -> Self {
        Range {
            start: self.end,
            end: self.start,
        }
    }

    fn overlaps(&self, other: &Range) -> bool {
        self.contains(other.start)
            || self.contains(other.end)
            || other.contains(self.start)
            || other.contains(self.end)
    }

    fn contains(&self, point: usize) -> bool {
        self.start <= point && self.end >= point || self.end <= point && self.start >= point
    }
}

impl From<std::ops::Range<usize>> for Range {
    fn from(range: std::ops::Range<usize>) -> Self {
        Range {
            start: range.start,
            end: range.end,
        }
    }
}

fn partiton_jumps(intervals: &[Range]) -> Vec<Vec<Range>> {
    let mut layers: Vec<Vec<Range>> = vec![vec![]];

    for &interval in intervals {
        // start at top, find minimum free layer, insert there.

        let mut min_layer = layers.len();
        for (idx, layer) in (0..layers.len()).zip(layers.iter()).rev() {
            // Merging into a layer is possible if the layer is empty or the last item does
            // not overlap with the current one
            if layer
                .last()
                .map(|&last| !last.overlaps(&interval))
                .unwrap_or(true)
            {
                min_layer = idx;
            }
        }

        if min_layer == layers.len() {
            layers.push(vec![interval]);
        } else {
            layers[min_layer].push(interval);
        }
    }

    layers
}

fn print_code(code: &Code) {
    use crate::code::Instruction;
    use std::collections::HashMap;

    let mut instructions = Vec::with_capacity(code.code.len());
    code::parse_instructions_into(&code.code, |entry| {
        instructions.push((
            entry.start,
            code::INSTRUCTION_NAMES[entry.tag as usize],
            entry.instruction,
        ));
    });

    // map of target instruction -> source instructions
    let mut branch_targets = HashMap::new();
    // map of source instruction -> target instructions
    let mut branch_sources = HashMap::new();
    // list of intervals covering [source_addr, target_addr]
    let mut intervals = Vec::new();
    for &(start, _, ref instruction) in &instructions {
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
                branch_targets
                    .entry(dest)
                    .or_insert_with(|| vec![])
                    .push(start);

                branch_sources
                    .entry(start)
                    .or_insert_with(|| vec![])
                    .push(dest);

                intervals.push((src..dest).into());
            }

            Instruction::Goto(off) => {
                let src = start;
                let dest = (istart + *off as isize) as usize;
                branch_targets
                    .entry(dest)
                    .or_insert_with(|| vec![])
                    .push(start);

                branch_sources
                    .entry(start)
                    .or_insert_with(|| vec![])
                    .push(dest);

                intervals.push((src..dest).into());
            }
            // Instruction::Ret(_) => (),
            _ => (),
        }
    }

    let layers = partiton_jumps(&intervals);

    let styles = [
        ansi_term::Style::default(),
        ansi_term::Style::default().dimmed(),
    ];

    let print_jmps = |idx| {
        for slot in (0..layers.len()).rev() {
            let mut was_printed = false;
            let mut print = |v| {
                if !was_printed {
                    print!("{}", styles[slot % styles.len()].paint(v));
                    was_printed = true;
                }
            };

            let layer = &layers[slot];
            for range in layer {
                if idx == range.start {
                    print("<");
                }
                if idx == range.end {
                    print(">");
                }
                if range.contains(idx) {
                    print("|");
                }
            }

            // Fill the column with *something*
            if !was_printed {
                print!(" ");
            }
        }
    };

    for &(start_byte, name, ref instruction) in &instructions {
        pad(1);

        let style = match instruction {
            // Instruction::Nop,
            Instruction::LoadInt(_)
            | Instruction::LoadLong(_)
            | Instruction::LoadFloat(_)
            | Instruction::LoadDouble(_)
            | Instruction::LoadRef(_)
            | Instruction::StoreInt(_)
            | Instruction::StoreLong(_)
            | Instruction::StoreFloat(_)
            | Instruction::StoreDouble(_)
            | Instruction::StoreRef(_)
            | Instruction::LoadArrayInt
            | Instruction::LoadArrayLong
            | Instruction::LoadArrayFloat
            | Instruction::LoadArrayDouble
            | Instruction::LoadArrayRef
            | Instruction::LoadArrayBool
            | Instruction::LoadArrayChar
            | Instruction::LoadArrayShort
            | Instruction::StoreArrayInt
            | Instruction::StoreArrayLong
            | Instruction::StoreArrayFloat
            | Instruction::StoreArrayDouble
            | Instruction::StoreArrayRef
            | Instruction::StoreArrayBool
            | Instruction::StoreArrayChar
            | Instruction::StoreArrayShort
            | Instruction::ConstNull
            | Instruction::ConstInt(_)
            | Instruction::ConstLong(_)
            | Instruction::ConstFloat(_)
            | Instruction::LoadConstant(_)
            | Instruction::ConstDouble(_) => style::OPCODE_LOAD_STORE,

            Instruction::PushByte(_)
            | Instruction::PushShort(_)
            | Instruction::Pop
            | Instruction::Pop2
            | Instruction::Dup
            | Instruction::DupX1
            | Instruction::DupX2
            | Instruction::Dup2
            | Instruction::Dup2X1
            | Instruction::Dup2X2
            | Instruction::Swap => style::OPCODE_OPERAND_STACK_MANAGEMENT,

            Instruction::AddInt
            | Instruction::AddLong
            | Instruction::AddFloat
            | Instruction::AddDouble
            | Instruction::SubInt
            | Instruction::SubLong
            | Instruction::SubFloat
            | Instruction::SubDouble
            | Instruction::MulInt
            | Instruction::MulLong
            | Instruction::MulFloat
            | Instruction::MulDouble
            | Instruction::DivInt
            | Instruction::DivLong
            | Instruction::DivFloat
            | Instruction::DivDouble
            | Instruction::RemInt
            | Instruction::RemLong
            | Instruction::RemFloat
            | Instruction::RemDouble
            | Instruction::NegInt
            | Instruction::NegLong
            | Instruction::NegFloat
            | Instruction::NegDouble
            | Instruction::ShlInt
            | Instruction::ShlLong
            | Instruction::ShrInt
            | Instruction::ShrLong
            | Instruction::LogicalShrInt
            | Instruction::LogicalShrLong
            | Instruction::AndInt
            | Instruction::AndLong
            | Instruction::OrInt
            | Instruction::OrLong
            | Instruction::XorInt
            | Instruction::XorLong
            | Instruction::IncInt
            | Instruction::CompareLong
            | Instruction::CompareLessFloat
            | Instruction::CompareGreaterFloat
            | Instruction::CompareLessDouble
            | Instruction::CompareGreaterDouble => style::OPCODE_ARITH_LOGIC,

            Instruction::IntToLong
            | Instruction::IntToFloat
            | Instruction::IntToDouble
            | Instruction::LongToInt
            | Instruction::LongToFloat
            | Instruction::LongToDouble
            | Instruction::FloatToInt
            | Instruction::FloatToLong
            | Instruction::FloatToDouble
            | Instruction::DoubleToInt
            | Instruction::DoubleToLong
            | Instruction::DoubleToFloat
            | Instruction::IntToBool
            | Instruction::IntToChar
            | Instruction::IntToShort => style::OPCODE_TYPE_CONVERSION,

            Instruction::IfEqual(_)
            | Instruction::IfNotEqual(_)
            | Instruction::IfLessThan(_)
            | Instruction::IfGreaterThan(_)
            | Instruction::IfLessThanEqual(_)
            | Instruction::IfGreaterThanEqual(_)
            | Instruction::IfEqualInt(_)
            | Instruction::IfNotEqualInt(_)
            | Instruction::IfLessThanInt(_)
            | Instruction::IfGreaterThanEqualInt(_)
            | Instruction::IfGreaterThanInt(_)
            | Instruction::IfLessThanEqualInt(_)
            | Instruction::IfEqualRef(_)
            | Instruction::IfNotEqualRef(_)
            | Instruction::IfNull(_)
            | Instruction::IfNonNull(_)
            | Instruction::Goto(_)
            | Instruction::Jsr(_)
            | Instruction::Ret(_)
            | Instruction::Tableswitch
            | Instruction::Lookupswitch => style::OPCODE_CONTROL_FLOW,

            Instruction::ReturnInt
            | Instruction::ReturnLong
            | Instruction::ReturnFloat
            | Instruction::ReturnDouble
            | Instruction::ReturnRef
            | Instruction::ReturnVoid
            | Instruction::InvokeVirtual(_)
            | Instruction::InvokeSpecial(_)
            | Instruction::InvokeStatic(_)
            | Instruction::InvokeInterface(_, _)
            | Instruction::InvokeDynamic(_) => style::OPCODE_INVOCATION_RETURN,

            Instruction::GetStatic(_)
            | Instruction::PutStatic(_)
            | Instruction::GetField(_)
            | Instruction::PutField(_)
            | Instruction::New(_)
            | Instruction::NewArrayPrimitive(_)
            | Instruction::NewArrayRef(_)
            | Instruction::NewArrayMultiRef(_, _) => style::OPCODE_OBJECT_MANIPULATION,

            _ => style::OPCODE_DEFAULT,
        };

        // TODO: mark branch/goto targets in some cool way
        // TODO: spacing for byte positions
        // TODO: code after method?

        // ArrayLength,
        // Throw,
        // CheckCast(u16),
        // InstanceOf(u16),
        // MonitorEnter,
        // MonitorExit,
        // Wide,

        // Breakpoint,
        // Impdep1,
        // Impdep2,

        print!("{} ", style::COMMENT.paint("//"),);

        print_jmps(start_byte);

        print!(" {}: {} ", start_byte, style.paint(name));

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

            Instruction::InvokeInterface(idx, len) => (),
            Instruction::NewArrayMultiRef(idx, len) => (),

            Instruction::NewArrayPrimitive(ty) => (),

            _ => (),
        }

        println!();
    }
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
            print_constant(&class.pool, entry, 0);
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
