use ansi_term::{Color, Style};
use std::{borrow::Cow, collections::HashMap};

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

lazy_static::lazy_static! {
    pub static ref STYLE_MAP: HashMap<String, Style> = {
        macro_rules! entries {
            ($($name:expr => $rest:tt $val:expr,)*) => {
                &[
                    $(($name, entries!($rest $val)),)*
                ]
            };

            (base $style:expr) => {
                StyleNode::Base($style)
            };

            (inherit $overrides:expr) => {
                StyleNode::Inherit($overrides)
            };
        }

        let entries = entries! {
            "comment" => base Color::Cyan.normal(),
            "extends" => base Color::Purple.bold(),
            "super" => base Color::Purple.bold(),
            "path" => base Color::Green.normal(),
            "throws" => base Color::Purple.bold(),

            "type" => base Color::Blue.bold(),
            "type.object" => inherit Overrides::default(),
            "type.object.parameter" => base Color::Yellow.bold(),
            "type.object.wildcard" => base Color::Yellow.bold(),
            "type.primitive" => inherit Overrides::default().bold(false),
            "type.primitive.byte" => inherit Overrides::default(),
            "type.primitive.char" => inherit Overrides::default(),
            "type.primitive.double" => inherit Overrides::default(),
            "type.primitive.float" => inherit Overrides::default(),
            "type.primitive.int" => inherit Overrides::default(),
            "type.primitive.long" => inherit Overrides::default(),
            "type.primitive.short" => inherit Overrides::default(),
            "type.primitive.boolean" => inherit Overrides::default(),
            "type.primitive.void" => inherit Overrides::default(),

            "pool.index" => base Color::Cyan.bold(),

            "pool.val" => base DEFAULT,
            "pool.val.int" => inherit Overrides::default(),
            "pool.val.float" => inherit Overrides::default(),
            "pool.val.long" => inherit Overrides::default(),
            "pool.val.double" => inherit Overrides::default(),
            "pool.val.string" => base Color::Green.normal(),

            "pool.ref" => base DEFAULT,
            "pool.ref.member" => base Color::Yellow.normal(),
            "pool.ref.member.field" => inherit Overrides::default(),
            "pool.ref.member.method" => inherit Overrides::default(),
            "pool.ref.member.interface_method" => inherit Overrides::default(),
            "pool.ref.string" => inherit Overrides::default(),
            "pool.ref.class" => inherit Overrides::default(),
            "pool.ref.name_and_type" => inherit Overrides::default(),
            "pool.ref.method_type" => inherit Overrides::default(),
            "pool.ref.method_handle" => inherit Overrides::default(),
            "pool.ref.invoke_dynamic" => inherit Overrides::default(),

            // "access.visibility" => base Color::Yellow.normal(),
            "access.visibility.public" => inherit Overrides::default(),
            "access.visibility.protected" => inherit Overrides::default(),
            "access.visibility.private" => inherit Overrides::default(),

            "access" => base Color::Purple.bold(),
            "access.other.final" => inherit Overrides::default(),
            "access.other.static" => inherit Overrides::default(),
            "access.other.abstract" => inherit Overrides::default(),

            "access.field.volatile" => inherit Overrides::default(),
            "access.field.transient" => inherit Overrides::default(),

            "access.method.synchronized" => inherit Overrides::default(),
            "access.method.strictfp" => inherit Overrides::default(),
            "access.method.native" => inherit Overrides::default(),

            "access.class" => base Color::Purple.bold(),
            "access.class.enum" => inherit Overrides::default(),
            "access.class.interface" => inherit Overrides::default(),
            "access.class.class" => inherit Overrides::default(),
            "access.class.annotation" => inherit Overrides::default(),

            "flow.branch.forward" => base Color::Green.normal(),
            "flow.branch.backward" => base Color::Cyan.bold(),
            "flow.jump.forward" => base Color::Yellow.normal(),
            "flow.jump.backward" => base Color::Red.bold(),

            "opcode.type" => base Color::Cyan.normal(),
            "opcode.type.load" => base Color::Cyan.normal(),
            "opcode.type.load.const" => inherit Overrides::default(),
            "opcode.type.store" => base Color::Cyan.normal(),

            "opcode.type.stack" => base Color::Yellow.normal(),
            "opcode.type.object" => base Color::Green.normal(),
            "opcode.type.arith" => base Color::Green.normal(),
            "opcode.type.logic" => base Color::Green.normal(),
            "opcode.type.conversion" => base Color::Green.normal(),

            "opcode.type.flow" => base Color::Yellow.normal().underline(),
            "opcode.type.flow.invocation" => inherit Overrides::default().underline(false),

            "opcode.immediate" => base Color::Yellow.normal(),
            "opcode.immediate.index" => inherit Overrides::default(),
            "opcode.immediate.branch" => inherit Overrides::default(),
            "opcode.immediate.other" => inherit Overrides::default(),
        };

        let entries = entries.into_iter().map(|&(name, node)| (Cow::from(name), node)).collect::<HashMap<_, _>>();
        let mut resolved = HashMap::new();

        for (path, _) in &entries {
            resolved.insert(path.clone().into(), compute_node(&entries, path).unwrap_or(DEFAULT));
        }

        resolved
    };
}

fn compute_node(ctx: &HashMap<Cow<'_, str>, StyleNode>, name: &str) -> Option<Style> {
    match ctx.get(name).cloned() {
        Some(StyleNode::Base(style)) => Some(style),
        Some(StyleNode::Inherit(overrides)) => {
            let (pos, _) = name.rmatch_indices(".").next()?;
            compute_node(ctx, &name[..pos]).map(|style| overrides.apply(style))
        }
        None => {
            let (pos, _) = name.rmatch_indices(".").next()?;
            compute_node(ctx, &name[..pos])
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct Overrides {
    pub foreground: Option<Option<Color>>,
    pub background: Option<Option<Color>>,
    pub is_bold: Option<bool>,
    pub is_dimmed: Option<bool>,
    pub is_italic: Option<bool>,
    pub is_underline: Option<bool>,
    pub is_blink: Option<bool>,
    pub is_reverse: Option<bool>,
    pub is_hidden: Option<bool>,
    pub is_strikethrough: Option<bool>,
}

impl Overrides {
    pub fn fg(color: Option<Color>) -> Self {
        Overrides {
            foreground: Some(color),
            ..Default::default()
        }
    }

    pub fn bold(self, bold: bool) -> Self {
        Overrides {
            is_bold: Some(bold),
            ..self
        }
    }

    pub fn dimmed(self, dimmed: bool) -> Self {
        Overrides {
            is_dimmed: Some(dimmed),
            ..self
        }
    }

    pub fn underline(self, underline: bool) -> Self {
        Overrides {
            is_underline: Some(underline),
            ..self
        }
    }

    fn apply(self, style: Style) -> Style {
        Style {
            foreground: self.foreground.unwrap_or(style.foreground),
            background: self.background.unwrap_or(style.background),
            is_bold: self.is_bold.unwrap_or(style.is_bold),
            is_dimmed: self.is_dimmed.unwrap_or(style.is_dimmed),
            is_italic: self.is_italic.unwrap_or(style.is_italic),
            is_underline: self.is_underline.unwrap_or(style.is_underline),
            is_blink: self.is_blink.unwrap_or(style.is_blink),
            is_reverse: self.is_reverse.unwrap_or(style.is_reverse),
            is_hidden: self.is_hidden.unwrap_or(style.is_hidden),
            is_strikethrough: self.is_strikethrough.unwrap_or(style.is_strikethrough),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StyleNode {
    Base(Style),
    Inherit(Overrides),
}
