pub mod attribute;
pub mod class;
pub mod constant;
pub mod field;
pub mod method;
mod mutf8;
pub mod signature;

pub use crate::mutf8::*;

use byteorder::{BigEndian, ByteOrder};

pub type ClassResult<T> = Result<T, ClassError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ClassError {
    Parse(ParseError),

    // Constant pool parsing errors
    UnknownConstantTag(u8),
    UnknownMethodHandleType(u8),
    ConstantPoolTooSmall,

    // Class parse errors
    WrongMagic,

    // Descriptor parse errors
    BadDescriptorType(u8),

    // Attribute parse errors
    UnknownVerificationType(u8),
    UnknownStackMapFrameType(u8),

    // General errors
    InvalidPoolIndex,
    InvalidPoolType,

    InvalidModifiedUtf8Byte(usize, u8),
}

impl From<ParseError> for ClassError {
    fn from(err: ParseError) -> Self {
        ClassError::Parse(err)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ParseError {
    Incomplete(usize),
    IncompleteUnknown,
    Error(usize),
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ByteParser<'src> {
    pub src: &'src [u8],
    pub offset: usize,
}

impl<'src> ByteParser<'src> {
    pub fn new(src: &'src [u8]) -> Self {
        ByteParser { src, offset: 0 }
    }

    pub fn remaining(&self) -> usize {
        self.src.len() - self.offset
    }

    pub fn tag(&mut self, tag: &[u8]) -> ParseResult<()> {
        if self.remaining() < tag.len() {
            Err(ParseError::Incomplete(tag.len() - self.remaining()))
        } else {
            for i in 0..tag.len() {
                if self.src[self.offset + i] != tag[i] {
                    return Err(ParseError::Error(self.offset + i));
                }
            }

            self.offset += tag.len();
            Ok(())
        }
    }

    pub fn take(&mut self, len: usize) -> ParseResult<&'src [u8]> {
        if self.remaining() < len {
            Err(ParseError::Incomplete(len - self.remaining()))
        } else {
            let res = &self.src[self.offset..self.offset + len];
            self.offset += len;
            Ok(res)
        }
    }

    /// Take bytes until a condition is no longer met. Note that `take_while`
    /// will consume the last inspected byte! That is,
    /// `ByteParser::new(b"aaaab").take_while(|c| c != b'b')` will consume the
    /// entire input! Additionally, the parser will return an error if the end
    /// of the input stream is reached while the predicate has not yet returned
    /// `false`.
    pub fn take_while<F>(&mut self, mut func: F) -> ParseResult<&'src [u8]>
    where
        F: FnMut(u8) -> bool,
    {
        let mut len = 0;
        // While we haven't run off the end of the buffer...
        while self.src.len() - self.offset - len > 0 {
            // if the condition is no longer met, then we take what we've seen
            // and return it
            if !func(self.src[self.offset + len]) {
                return self.take(len + 1);
            }

            len += 1;
        }

        Err(ParseError::IncompleteUnknown)
    }

    pub fn peek(&mut self, len: usize) -> ParseResult<&'src [u8]> {
        if self.remaining() < len {
            Err(ParseError::Incomplete(len - self.remaining()))
        } else {
            let res = &self.src[self.offset..self.offset + len];
            Ok(res)
        }
    }

    pub fn seq<F, T, E>(&mut self, len: usize, mut func: F) -> Result<Vec<T>, E>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
    {
        let mut vec = Vec::with_capacity(len);
        for _ in 0..len {
            vec.push(func(self)?);
        }
        Ok(vec)
    }

    pub fn parse_u8(&mut self) -> ParseResult<u8> {
        self.take(1).map(|b| b[0])
    }

    pub fn parse_u16(&mut self) -> ParseResult<u16> {
        self.take(2).map(BigEndian::read_u16)
    }

    pub fn parse_u32(&mut self) -> ParseResult<u32> {
        self.take(4).map(BigEndian::read_u32)
    }

    pub fn parse_u64(&mut self) -> ParseResult<u64> {
        self.take(8).map(BigEndian::read_u64)
    }

    pub fn parse_i8(&mut self) -> ParseResult<i8> {
        self.take(1).map(|b| b[0] as i8)
    }

    pub fn parse_i16(&mut self) -> ParseResult<i16> {
        self.take(2).map(|b| BigEndian::read_u16(b) as i16)
    }

    pub fn parse_i32(&mut self) -> ParseResult<i32> {
        self.take(4).map(|b| BigEndian::read_u32(b) as i32)
    }

    pub fn parse_i64(&mut self) -> ParseResult<i64> {
        self.take(8).map(|b| BigEndian::read_u64(b) as i64)
    }

    pub fn parse_f32(&mut self) -> ParseResult<f32> {
        self.take(4).map(|b| f32::from_bits(BigEndian::read_u32(b)))
    }

    pub fn parse_f64(&mut self) -> ParseResult<f64> {
        self.take(8).map(|b| f64::from_bits(BigEndian::read_u64(b)))
    }
}
