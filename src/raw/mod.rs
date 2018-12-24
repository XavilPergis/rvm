pub mod attribute;
pub mod class;
pub mod constant;
pub mod field;
pub mod method;

use byteorder::{BigEndian, ByteOrder};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ParseError {
    Incomplete(usize),
    Error(usize),
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct ByteParser<'src> {
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

    pub fn seq<F, T, E>(&mut self, len: usize, mut func: F) -> Result<Vec<T>, E>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
        E: From<ParseError>,
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
