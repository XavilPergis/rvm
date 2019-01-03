//! Utilities for grabbing strings, or formatting data from a class constant
//! pool.

use class::constant::Constant;

pub const WRONG_TYPE_MESSAGE: &str = "<wrong type>";

pub fn get_str(pool: &[Constant], index: usize) -> &str {
    pool[index].as_string_data().unwrap_or(WRONG_TYPE_MESSAGE)
}

pub fn get_class_name(pool: &[Constant], index: usize) -> &str {
    match &pool[index] {
        &Constant::Class(index) => get_str(pool, index),
        _ => WRONG_TYPE_MESSAGE,
    }
}
