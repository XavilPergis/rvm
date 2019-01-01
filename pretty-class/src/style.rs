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
        let entries = &[
            ("comment", StyleNode::Base(Color::Cyan.normal())),
            ("extends", StyleNode::Base(Color::Yellow.bold())),

            ("type", StyleNode::Base(Color::Green.normal())),
            ("type.object", StyleNode::Inherit(Overrides::default().bold(true))),
            ("type.primitive", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.byte", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.char", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.double", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.float", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.int", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.long", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.short", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.boolean", StyleNode::Inherit(Overrides::default())),
            ("type.primitive.void", StyleNode::Inherit(Overrides::default())),

            ("pool.index", StyleNode::Base(Color::Cyan.bold())),

            ("pool.val", StyleNode::Base(DEFAULT)),
            ("pool.val.int", StyleNode::Inherit(Overrides::default())),
            ("pool.val.float", StyleNode::Inherit(Overrides::default())),
            ("pool.val.long", StyleNode::Inherit(Overrides::default())),
            ("pool.val.double", StyleNode::Inherit(Overrides::default())),
            ("pool.val.string", StyleNode::Base(Color::Green.normal())),

            ("pool.ref", StyleNode::Base(DEFAULT)),
            ("pool.ref.member", StyleNode::Base(Color::Yellow.normal())),
            ("pool.ref.member.field", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.member.method", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.member.interface_method", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.string", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.class", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.name_and_type", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.method_type", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.method_handle", StyleNode::Inherit(Overrides::default())),
            ("pool.ref.invoke_dynamic", StyleNode::Inherit(Overrides::default())),

            ("access.visibility", StyleNode::Base(Color::Yellow.normal())),
            ("access.visibility.public", StyleNode::Inherit(Overrides::default())),
            ("access.visibility.protected", StyleNode::Inherit(Overrides::default())),
            ("access.visibility.private", StyleNode::Inherit(Overrides::default())),

            ("access.other", StyleNode::Base(Color::Yellow.normal())),
            ("access.other.final", StyleNode::Inherit(Overrides::default())),
            ("access.other.static", StyleNode::Inherit(Overrides::default())),
            ("access.other.abstract", StyleNode::Inherit(Overrides::default())),

            ("access.field.volatile", StyleNode::Inherit(Overrides::default())),
            ("access.field.transient", StyleNode::Inherit(Overrides::default())),

            ("access.method.synchronized", StyleNode::Inherit(Overrides::default())),
            ("access.method.strictfp", StyleNode::Inherit(Overrides::default())),
            ("access.method.native", StyleNode::Inherit(Overrides::default())),

            ("access.class", StyleNode::Base(Color::Blue.normal())),
            ("access.class.enum", StyleNode::Inherit(Overrides::default())),
            ("access.class.interface", StyleNode::Inherit(Overrides::default())),
            ("access.class.class", StyleNode::Inherit(Overrides::default())),
            ("access.class.annotation", StyleNode::Inherit(Overrides::default())),

            ("flow.branch.forward", StyleNode::Base(Color::Green.normal())),
            ("flow.branch.backward", StyleNode::Base(Color::Cyan.bold())),
            ("flow.jump.forward", StyleNode::Base(Color::Yellow.normal())),
            ("flow.jump.backward", StyleNode::Base(Color::Red.bold())),

            ("opcode.type", StyleNode::Base(Color::Cyan.normal())),
            ("opcode.type.load", StyleNode::Base(Color::Cyan.normal())),
            ("opcode.type.load.const", StyleNode::Inherit(Overrides::default())),
            ("opcode.type.store", StyleNode::Base(Color::Cyan.normal())),

            ("opcode.type.stack", StyleNode::Base(Color::Yellow.normal())),
            ("opcode.type.object", StyleNode::Base(Color::Green.normal())),
            ("opcode.type.arith", StyleNode::Base(Color::Green.normal())),
            ("opcode.type.logic", StyleNode::Base(Color::Green.normal())),
            ("opcode.type.conversion", StyleNode::Base(Color::Green.normal())),

            ("opcode.type.flow", StyleNode::Base(Color::Yellow.normal().underline())),
            ("opcode.type.flow.invocation", StyleNode::Inherit(Overrides::default().underline(false))),

            ("opcode.immediate", StyleNode::Base(Color::Yellow.normal())),
            ("opcode.immediate.index", StyleNode::Inherit(Overrides::default())),
            ("opcode.immediate.branch", StyleNode::Inherit(Overrides::default())),
            ("opcode.immediate.other", StyleNode::Inherit(Overrides::default())),
        ];

        let entries = entries.into_iter().map(|&(name, node)| (Cow::from(name), node)).collect::<HashMap<_, _>>();
        let mut resolved = HashMap::new();

        for (path, _) in &entries {
            resolved.insert(path.clone().into(), compute_node(&entries, path).unwrap_or(DEFAULT));
        }

        resolved
    };
}

fn compute_node(ctx: &HashMap<Cow<'_, str>, StyleNode>, name: &str) -> Option<Style> {
    match *ctx.get(name)? {
        StyleNode::Base(style) => Some(style),
        StyleNode::Inherit(overrides) => {
            let (pos, _) = name.rmatch_indices(".").next()?;
            compute_node(ctx, &name[..pos]).map(|style| overrides.apply(style))
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
