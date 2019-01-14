use crate::{
    code::{def::InstructionEntry, Range},
    style::STYLE_MAP,
};
use ::class::{
    attribute::{Attribute, AttributeInfo, Code},
    class::{self, Class},
    constant::Constant,
    field, method,
    parse::ByteParser,
    signature as sig,
};
use std::{fs::File, io::Read, path::PathBuf};
use structopt::StructOpt;

pub mod code;
pub mod constant;
pub mod pool;
pub mod signature;
pub mod style;

fn pad(count: usize) {
    for _ in 0..count {
        print!("  ");
    }
}

fn print_attribute(class: &Class, attr: &AttributeInfo, depth: usize) {
    if !APP.show_attributes {
        return;
    }

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
            if APP.show_code {
                print_code(&class.pool, code);
            }
            add_newline = false;
        }

        Attribute::StackMapTable(_) => print!("StackMapTable"),
        Attribute::ConstantValue(idx) => {
            if constant::print_constant_index(&class.pool, *idx) {
                print!(" ");
            }
            constant::print_constant_value(&class.pool, *idx);
        }
        Attribute::Signature(idx) => print!("{}", pool::get_str(&class.pool, *idx)),
        Attribute::Exceptions(exceptions) => {
            if exceptions.len() > 0 {
                constant::print_constant_value(&class.pool, exceptions[0]);
                for &idx in &exceptions[1..] {
                    print!(", ");
                    constant::print_constant_value(&class.pool, idx);
                }
            }
        }
    }

    if add_newline {
        println!();
    }
}

use ::class::access::*;

fn print_class_properties(props: &ClassProperties) {
    let mut was_written = false;
    let mut write = |s, col| {
        if was_written {
            APP.paint(col, || print!(" {}", s));
        } else {
            APP.paint(col, || print!("{}", s));
            was_written = true;
        }
    };

    if props.access == Access::Public {
        write("public", "access.visibility.public");
    }

    match props.ty {
        ClassType::Class => {
            if props.is_final {
                write("final", "access.other.final");
            } else if props.is_abstract {
                write("abstract", "access.other.abstract");
            }
            write("class", "access.class.class")
        }
        ClassType::Enum => write("enum", "access.class.enum"),
        ClassType::Interface => write("interface", "access.class.interface"),
        ClassType::Annotation => write("@interface", "access.class.annotation"),
    }
}

fn print_field_properties(props: &FieldProperties) {
    let mut was_written = false;
    let mut write = |s, col| {
        if was_written {
            APP.paint(col, || print!(" {}", s));
        } else {
            APP.paint(col, || print!("{}", s));
            was_written = true;
        }
    };

    match props.access {
        Access::Public => write("public", "access.visibility.public"),
        Access::Protected => write("protected", "access.visibility.protected"),
        Access::Private => write("private", "access.visibility.private"),
        _ => (),
    }

    if props.is_static {
        write("static", "access.other.static");
    }

    if props.is_final {
        write("final", "access.other.final");
    } else if props.is_transient {
        write("transient", "access.field.transient");
    }

    if props.is_volatile {
        write("volatile", "access.field.volatile");
    }

    if was_written {
        print!(" ");
    }
}

fn print_method_properties(props: &MethodProperties) {
    let mut was_written = false;
    let mut write = |s, col| {
        if was_written {
            APP.paint(col, || print!(" {}", s));
        } else {
            APP.paint(col, || print!("{}", s));
            was_written = true;
        }
    };

    // TODO: deduplicate code
    match props.access {
        Access::Public => write("public", "access.visibility.public"),
        Access::Protected => write("protected", "access.visibility.protected"),
        Access::Private => write("private", "access.visibility.private"),
        _ => (),
    }

    if props.is_abstract {
        write("abstract", "access.other.static");
    } else {
        if props.is_static {
            write("static", "access.other.static");
        }

        if props.is_final {
            write("final", "access.other.final");
        }

        if props.is_synchronized {
            write("synchronized", "access.method.synchronized");
        }

        if props.is_strict {
            write("strictfp", "access.method.strictfp");
        }

        if props.is_native {
            write("native", "access.method.native");
        }
    }

    if was_written {
        print!(" ");
    }
}

fn print_code(pool: &[Constant], code: &Code) {
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

        use crate::{code::def::ArrayPrimitiveType, constant::print_constant_value};

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
                APP.paint("opcode.immediate.index", || print!("{} ", idx));
                print_constant_value(pool, *idx as usize);
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
                APP.paint("opcode.immediate.index", || print!("{} ", idx));
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
                APP.paint("opcode.immediate.other", || print!("{} ", len));
                print_constant_value(pool, *idx as usize);
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

    /// Print item attributes.
    #[structopt(short = "A", long = "attributes")]
    pub show_attributes: bool,

    /// Do not print the full path of an object; just the name.
    #[structopt(long = "no-paths")]
    pub no_show_paths: bool,
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

fn print_class_decl(class: &Class) {
    print_class_properties(&class.properties);

    print!(" ");

    let signature = get_signature_attrib(&class.pool, &class.attributes)
        .and_then(|raw| sig::parse_class_signature(&mut ByteParser::new(raw.as_bytes())).ok())
        .or_else(|| signature::class_to_class_sig(&class).ok());

    if let Some(signature) = signature {
        signature::print_class(&signature, class);
    } else {
        print!("<invalid class signature>");
    }
}

fn get_signature_attrib<'p>(pool: &'p [Constant], attributes: &[AttributeInfo]) -> Option<&'p str> {
    for attr in attributes {
        match &attr.attr {
            Attribute::Signature(idx) => return pool[*idx].as_string_data(),
            _ => (),
        }
    }

    None
}

use std::cmp::Ordering;

#[derive(Clone, Debug, Eq, PartialEq)]
struct FieldPropertiesOrdering(FieldProperties);

impl PartialOrd for FieldPropertiesOrdering {
    fn partial_cmp(&self, other: &FieldPropertiesOrdering) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FieldPropertiesOrdering {
    fn cmp(&self, other: &FieldPropertiesOrdering) -> Ordering {
        (other.0.is_enum.cmp(&self.0.is_enum))
            .then(other.0.is_static.cmp(&self.0.is_static))
            .then(
                self.0
                    .is_compiler_generated
                    .cmp(&other.0.is_compiler_generated),
            )
            .then(self.0.access.cmp(&other.0.access))
            .then(other.0.is_final.cmp(&self.0.is_final))
            .then(other.0.is_volatile.cmp(&self.0.is_volatile))
            .then(other.0.is_transient.cmp(&self.0.is_transient))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct MethodPropertiesOrdering(MethodProperties);

impl PartialOrd for MethodPropertiesOrdering {
    fn partial_cmp(&self, other: &MethodPropertiesOrdering) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for MethodPropertiesOrdering {
    fn cmp(&self, other: &MethodPropertiesOrdering) -> Ordering {
        (other.0.is_static.cmp(&self.0.is_static))
            .then(other.0.is_abstract.cmp(&self.0.is_abstract))
            .then(
                self.0
                    .is_compiler_generated
                    .cmp(&other.0.is_compiler_generated),
            )
            .then(self.0.access.cmp(&other.0.access))
            .then(other.0.is_final.cmp(&self.0.is_final))
            .then(other.0.is_synchronized.cmp(&self.0.is_synchronized))
            .then(other.0.is_native.cmp(&self.0.is_native))
            .then(other.0.is_strict.cmp(&self.0.is_strict))
    }
}

fn parse_class(buf: &[u8]) {
    let class = match Class::parse(buf) {
        Ok(class) => class,
        Err(err) => {
            println!("Invalid Class! Raw error: {:?}", err);
            return;
        }
    };

    if APP.show_constant_pool {
        for entry in 1..class.pool.len() {
            APP.paint("pool.index", || print!("{:5}", entry));
            print!(" = ");
            constant::print_constant(&class.pool, entry, 0);
        }
    }

    use ::class::signature::Type;
    if APP.show_decl {
        for attr in &*class.attributes {
            print_attribute(&class, &attr, 0);
        }

        APP.paint("comment", || {
            println!("// Version {}.{}", class.version.major, class.version.minor);
        });

        print_class_decl(&class);

        println!(" {{");

        let mut fields = class.fields.clone();
        fields.sort_by_key(|item| FieldPropertiesOrdering(item.properties));

        let mut methods = class.methods.clone();
        methods.sort_by_key(|item| MethodPropertiesOrdering(item.properties));

        let mut was_static = fields.iter().map(|f| f.properties.is_static).next();

        for field in &*fields {
            if Some(field.properties.is_static) != was_static {
                was_static = Some(field.properties.is_static);
                println!();
            }

            for attr in &*field.attributes {
                print_attribute(&class, &attr, 1);
            }

            pad(1);
            print_field_properties(&field.properties);
            let name = pool::get_str(&class.pool, field.name);

            let signature = get_signature_attrib(&class.pool, &*field.attributes)
                .and_then(|raw| {
                    sig::parse_field_type_signature(&mut ByteParser::new(raw.as_bytes()))
                        .map(Type::Reference)
                        .ok()
                })
                .or_else(|| signature::field_desc_to_ty(field.descriptor.clone()).ok());

            if let Some(ty) = signature {
                signature::print_type(&ty);
            } else {
                print!("<invalid field signature>");
            }

            println!(" {};", name);
        }

        println!();

        let mut was_static = methods.iter().map(|m| m.properties.is_static).next();

        for method in &*methods {
            if Some(method.properties.is_static) != was_static {
                was_static = Some(method.properties.is_static);
                println!();
            }

            for attr in &*method.attributes {
                print_attribute(&class, &attr, 1);
            }

            pad(1);
            print_method_properties(&method.properties);
            let name = pool::get_str(&class.pool, method.name);

            let signature = get_signature_attrib(&class.pool, &*method.attributes)
                .and_then(|raw| {
                    sig::parse_method_signature(&mut ByteParser::new(raw.as_bytes())).ok()
                })
                .or_else(|| signature::method_desc_to_sig(method.descriptor.clone()).ok());

            if let Some(ty) = signature {
                signature::print_method(&ty, name);
            } else {
                print!("<invalid method signature>");
            }

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
