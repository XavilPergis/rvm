use byteorder::{BigEndian, ByteOrder};

pub use crate::mutf8::*;

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

    /// Tries to run the function, and backtraces by setting self to the state
    /// of the parser before the function was run if an error was returned.
    ///
    /// It is important to note that using multiple parse functionss without
    /// surrounding them in a call to `backtrace` will likely result in
    /// incorrect behavior. That is, running a parser multiple times without
    /// backtracing will live the parser's head potentially pointing to the
    /// middle of some input instead of the beginning.
    pub fn backtrace<F, T, E>(&mut self, mut func: F) -> Result<T, E>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
    {
        let start = self.clone();
        func(self).map_err(|err| {
            *self = start;
            err
        })
    }

    /// Takes `tag.len()` bytes and returns an error if the bytes did not match.
    pub fn expect(&mut self, tag: &[u8]) -> ParseResult<()> {
        self.backtrace(|p| {
            if p.remaining() < tag.len() {
                Err(ParseError::Incomplete(tag.len() - p.remaining()))
            } else {
                for i in 0..tag.len() {
                    if p.src[p.offset + i] != tag[i] {
                        return Err(ParseError::Error(p.offset + i));
                    }
                }

                p.offset += tag.len();
                Ok(())
            }
        })
    }

    /// Takes `len` bytes, and errors if there were not enough bytes remaining.
    pub fn take(&mut self, len: usize) -> ParseResult<&'src [u8]> {
        self.backtrace(|p| {
            if p.remaining() < len {
                Err(ParseError::Incomplete(len - p.remaining()))
            } else {
                let res = &p.src[p.offset..p.offset + len];
                p.offset += len;
                Ok(res)
            }
        })
    }

    pub fn predicate_len<F>(&self, mut func: F) -> usize
    where
        F: FnMut(u8) -> bool,
    {
        let mut len = 0;
        // Increase len until we run out of bytes...
        while self.remaining() - len > 0 {
            // ...or we reach the end of the predicate
            if !func(self.src[self.offset + len]) {
                break;
            }

            len += 1;
        }
        len
    }

    /// Takes bytes until a condition is no longer met. Note that `take_while`
    /// will consume the last inspected byte! That is,
    /// `ByteParser::new(b"aaaab").take_while(|c| c != b'b')` will consume the
    /// entire input! Additionally, the parser will return an error if the end
    /// of the input stream is reached while the predicate has not yet returned
    /// `false`.
    pub fn take_while<F>(&mut self, func: F) -> ParseResult<&'src [u8]>
    where
        F: FnMut(u8) -> bool,
    {
        let len = self.predicate_len(func);
        if self.remaining() - len == 0 {
            Err(ParseError::IncompleteUnknown)
        } else {
            let out = self.take(len)?;
            self.offset += 1;
            Ok(out)
        }
    }

    /// Like `take_while`, but doesn't consume the last inspected byte.
    pub fn peeking_take_while<F>(&mut self, func: F) -> ParseResult<&'src [u8]>
    where
        F: FnMut(u8) -> bool,
    {
        let len = self.predicate_len(func);
        if self.remaining() - len == 0 {
            Err(ParseError::IncompleteUnknown)
        } else {
            self.take(len)
        }
    }

    /// Constructs a vector of the results of `func` by repeatedly applying it
    /// until it returns an `Err`. This is like `<production>*`.
    pub fn repeat0<F, T, E>(&mut self, mut func: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
    {
        let mut results = vec![];
        loop {
            match self.backtrace(|p| func(p)) {
                Ok(val) => results.push(val),
                Err(_) => break,
            }
        }
        results
    }

    /// Constructs a vector of the results of `func` by applying it once
    /// normally, and then operating like `repeat0` for the rest of the input.
    /// This is like `<production>+`.
    pub fn repeat1<F, T, E>(&mut self, mut func: F) -> Result<Vec<T>, E>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
    {
        let mut results = vec![self.backtrace(|p| func(p))?];
        loop {
            match self.backtrace(|p| func(p)) {
                Ok(val) => results.push(val),
                Err(_) => break,
            }
        }
        Ok(results)
    }

    /// Like `take`, but does not advance the parser.
    pub fn peek(&mut self, len: usize) -> ParseResult<&'src [u8]> {
        if self.remaining() < len {
            Err(ParseError::Incomplete(len - self.remaining()))
        } else {
            let res = &self.src[self.offset..self.offset + len];
            Ok(res)
        }
    }

    /// Applies `func` exactly `len` times, and returns a vector of the items
    /// produced by `func`.
    pub fn seq<F, T, E>(&mut self, len: usize, mut func: F) -> Result<Vec<T>, E>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
    {
        let mut vec = Vec::with_capacity(len);
        for _ in 0..len {
            vec.push(self.backtrace(|p| func(p))?);
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

#[cfg(test)]
mod tests {
    use super::*;

    // owo
    #[test]
    fn test_pred_len() {
        let mut parser = ByteParser::new(b"abcdefgh");
        parser.expect(b"abcd").unwrap();
        assert_eq!(4, parser.predicate_len(|_| true));
    }

    #[test]
    fn test_take_while_valid() {
        let mut parser = ByteParser::new(b"foo.bar");
        assert_eq!(parser.take_while(|ch| ch != b'.'), Ok(&b"foo"[..]));
        assert_eq!(parser.remaining(), 3);
    }

    #[test]
    fn test_peeking_take_while_valid() {
        let mut parser = ByteParser::new(b"foo.bar");
        assert_eq!(parser.peeking_take_while(|ch| ch != b'.'), Ok(&b"foo"[..]));
        assert_eq!(parser.remaining(), 4);
    }

    #[test]
    fn test_take_while_invalid() {
        let mut parser = ByteParser::new(b"foo");
        assert_eq!(
            parser.take_while(|ch| ch != b'.'),
            Err(ParseError::IncompleteUnknown)
        );
    }
}
