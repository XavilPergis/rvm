use ansi_term::{Color, Style};

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

const fn fg(style: Style, color: Color) -> Style {
    Style {
        foreground: Some(color),
        ..style
    }
}

const fn bold(style: Style) -> Style {
    Style {
        is_bold: true,
        ..style
    }
}

const fn underline(style: Style) -> Style {
    Style {
        is_underline: true,
        ..style
    }
}

pub const DATA_GROUP: Style = bold(fg(DEFAULT, Color::Green));
pub const INT: Style = DATA_GROUP;
pub const LONG: Style = DATA_GROUP;
pub const FLOAT: Style = DATA_GROUP;
pub const DOUBLE: Style = DATA_GROUP;
pub const STRING: Style = DEFAULT;

// Invocation and field metadata
pub const CLASS_REFERENCE_GROUP: Style = fg(bold(DEFAULT), Color::Yellow);
pub const FIELD_REF: Style = CLASS_REFERENCE_GROUP;
pub const METHOD_REF: Style = CLASS_REFERENCE_GROUP;
pub const INTERFACE_METHOD_REF: Style = CLASS_REFERENCE_GROUP;
pub const STRING_REF: Style = CLASS_REFERENCE_GROUP;
pub const CLASS_REF: Style = CLASS_REFERENCE_GROUP;

pub const NAME_AND_TYPE: Style = fg(bold(DEFAULT), Color::Blue);
pub const METHOD_TYPE: Style = fg(DEFAULT, Color::Purple);
pub const METHOD_HANDLE: Style = fg(DEFAULT, Color::Purple);
pub const INVOKE_DYNAMIC: Style = fg(DEFAULT, Color::Purple);

pub const ACCESS_VISIBILITY_GROUP: Style = fg(DEFAULT, Color::Yellow);
pub const ACCESS_PUBLIC: Style = ACCESS_VISIBILITY_GROUP;
pub const ACCESS_PROTECTED: Style = ACCESS_VISIBILITY_GROUP;
pub const ACCESS_PRIVATE: Style = ACCESS_VISIBILITY_GROUP;

pub const ACCESS_OTHER_QUALIFIER_GROUP: Style = fg(DEFAULT, Color::Yellow);
// Method and field
pub const ACCESS_FINAL: Style = ACCESS_OTHER_QUALIFIER_GROUP;
pub const ACCESS_STATIC: Style = ACCESS_OTHER_QUALIFIER_GROUP;
pub const ACCESS_ABSTRACT: Style = ACCESS_OTHER_QUALIFIER_GROUP;

// Field only
pub const ACCESS_VOLATILE: Style = ACCESS_OTHER_QUALIFIER_GROUP;
pub const ACCESS_TRANSIENT: Style = ACCESS_OTHER_QUALIFIER_GROUP;

// Method only
pub const ACCESS_SYNCHRONIZED: Style = ACCESS_OTHER_QUALIFIER_GROUP;
pub const ACCESS_STRICTFP: Style = ACCESS_OTHER_QUALIFIER_GROUP;
pub const ACCESS_NATIVE: Style = ACCESS_OTHER_QUALIFIER_GROUP;

pub const ACCESS_CLASS_TYPE: Style = fg(bold(DEFAULT), Color::Blue);
pub const ACCESS_ENUM: Style = ACCESS_CLASS_TYPE;
pub const ACCESS_INTERFACE: Style = ACCESS_CLASS_TYPE;
pub const ACCESS_CLASS: Style = ACCESS_CLASS_TYPE;
pub const ACCESS_ANNOTATION: Style = ACCESS_CLASS_TYPE;

pub const COMMENT: Style = fg(DEFAULT, Color::Cyan);
pub const INDEX: Style = fg(bold(DEFAULT), Color::Cyan);

pub const PRIMITIVE_TYPE: Style = fg(DEFAULT, Color::Green);
pub const CLASS_NAME: Style = bold(PRIMITIVE_TYPE);
pub const EXTENDS: Style = fg(bold(DEFAULT), Color::Yellow);

pub const OPCODE_LOAD_STORE: Style = fg(DEFAULT, Color::Cyan);
pub const OPCODE_OPERAND_STACK_MANAGEMENT: Style = fg(DEFAULT, Color::Yellow);

pub const OPCODE_OBJECT_MANIPULATION: Style = fg(DEFAULT, Color::Green);
pub const OPCODE_ARITH_LOGIC: Style = fg(DEFAULT, Color::Green);
pub const OPCODE_TYPE_CONVERSION: Style = fg(DEFAULT, Color::Green);

pub const OPCODE_CONTROL_FLOW: Style = fg(DEFAULT, Color::Yellow);
pub const OPCODE_INVOCATION_RETURN: Style = fg(underline(DEFAULT), Color::Yellow);

// Load and store (e.g. aload_0, istore)
// Arithmetic and logic (e.g. ladd, fcmpl)
// Type conversion (e.g. i2b, d2i)
// Object creation and manipulation (new, putfield)
// Operand stack management (e.g. swap, dup2)
// Control transfer (e.g. ifeq, goto)
// Method invocation and return (e.g. invokespecial, areturn)

pub const OPCODE_DEFAULT: Style = bold(DEFAULT);
pub const OPCODE_IMMEDIATE_INDEX: Style = fg(DEFAULT, Color::Yellow);
pub const OPCODE_IMMEDIATE_BRANCH: Style = fg(DEFAULT, Color::Yellow);
pub const OPCODE_IMMEDIATE_OTHER: Style = fg(DEFAULT, Color::Yellow);
