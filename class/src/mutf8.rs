use crate::{parse::ByteParser, ClassError, ClassResult};
use std::borrow::Cow;

// why did I make this so generic...
pub struct CowBuilder<'a, T>
where
    [T]: ToOwned,
{
    cow: Cow<'a, [T]>,
    pos: usize,
}

impl<'a, T> CowBuilder<'a, T>
where
    [T]: ToOwned,
{
    pub fn new<C: Into<Cow<'a, [T]>>>(cow: C) -> Self {
        CowBuilder {
            cow: cow.into(),
            pos: 0,
        }
    }

    pub fn into_inner(self) -> Cow<'a, [T]> {
        self.cow
    }
}

impl<T> Extend<T> for CowBuilder<'_, T>
where
    [T]: ToOwned,
    <[T] as ToOwned>::Owned: Extend<T>,
    T: PartialEq + Clone,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for item in iter.into_iter() {
            self.push(item);
        }
    }
}

impl<'a, T> CowBuilder<'a, T>
where
    [T]: ToOwned,
    <[T] as ToOwned>::Owned: Extend<T>,
    T: PartialEq + Clone,
{
    // If the streams are in-sync, then we don't need to allocate
    pub fn push(&mut self, item: T) -> Option<T> {
        match &mut self.cow {
            // If we already have the owned variant, then we don't need to keep track of the input
            // stream at all, since we don't need to compare against an item from the input.
            Cow::Owned(vec) => {
                vec.extend(std::iter::once(item));
                return None;
            }

            Cow::Borrowed(slice) => {
                if &slice[self.pos] == &item {
                    self.pos += 1;
                    return Some(item);
                }
            }
        }

        // If we get here, we have a borrowed representation that is out-of-sync. We need to allocate a buffer, and copy the input stream up to the current offset
        self.cow = Cow::Owned({
            let mut owned = self.cow[..self.pos].to_owned();
            owned.extend(std::iter::once(item));
            owned
        });

        None
    }
}

pub fn parse_mutf8(bytes: &[u8]) -> ClassResult<Cow<'_, str>> {
    let mut parser = ByteParser::new(bytes);
    let mut builder = CowBuilder::new(bytes);

    macro_rules! invalid {
        ($byte:expr) => {
            return Err(ClassError::InvalidModifiedUtf8Byte(
                parser.offset - 1,
                $byte,
            ));
        };
    }

    while let Ok(b1) = parser.parse_u8() {
        if b1 == 0 || b1 >= 0xf0 {
            invalid!(b1);
        }

        // One-byte ASCII, is already UTF-8, just pass it through
        if b1 & 0x80 == 0 {
            // 0xxxxxxx
            builder.push(b1);
        } else if (b1 & 0xe0) ^ 0xc0 == 0 {
            // 110xxxxx 10xxxxxx
            let b2 = parser.parse_u8()?;
            if (b2 & 0xc0) ^ 0x80 != 0 {
                invalid!(b2);
            }

            // If this is a NUL codepoint, push the correct UTF-8 encoding instead of the
            // overlong encoding
            if b1 == 0xc0 && b2 == 0x80 {
                builder.push(0);
            } else {
                builder.push(b1);
                builder.push(b2);
            }
        } else if b1 == 0xed {
            // 11101101 1010xxxx 10xxxxxx 11101101 1011yyyy 10yyyyyy
            let b2 = parser.parse_u8()?;
            if (b2 & 0xf0) ^ 0xa0 != 0 {
                invalid!(b2);
            }

            let b3 = parser.parse_u8()?;
            if (b3 & 0xc0) ^ 0x80 != 0 {
                invalid!(b3);
            }

            let b4 = parser.parse_u8()?;
            if b4 != 0xed {
                invalid!(b4);
            }

            let b5 = parser.parse_u8()?;
            if (b5 & 0xf0) ^ 0xb0 != 0 {
                invalid!(b5);
            }

            let b6 = parser.parse_u8()?;
            if (b6 & 0xc0) ^ 0x80 != 0 {
                invalid!(b6);
            }

            let scalar = std::char::from_u32(
                0x10000
                    + ((b2 as u32 & 0x0f) << 16)
                    + ((b3 as u32 & 0x3f) << 10)
                    + ((b5 as u32 & 0x0f) << 6)
                    + (b6 as u32 & 0x3f),
            )
            .unwrap();

            let mut utf8 = [0; 4];
            scalar.encode_utf8(&mut utf8);
            builder.extend(utf8.iter().cloned());
        } else if (b1 & 0xf0) ^ 0xe0 == 0 {
            // 1110xxxx 10xxxxxx 10xxxxxx
            let b2 = parser.parse_u8()?;
            if (b2 & 0xc0) ^ 0x80 != 0 {
                invalid!(b2);
            }

            let b3 = parser.parse_u8()?;
            if (b3 & 0xc0) ^ 0x80 != 0 {
                invalid!(b2);
            }

            builder.push(b1);
            builder.push(b2);
            builder.push(b3);
        } else {
            invalid!(b1);
        }
    }

    Ok(match builder.into_inner() {
        Cow::Owned(vec) => String::from_utf8(vec).unwrap().into(),
        Cow::Borrowed(vec) => std::str::from_utf8(vec).unwrap().into(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mutf8_normal() {
        let string = "Hello, world!";
        match parse_mutf8(string.as_bytes()) {
            Ok(Cow::Borrowed(text)) => assert_eq!(text, string),
            other => panic!(other),
        }
    }

    #[test]
    fn test_mutf8_non_latin() {
        let string = "Здравствуй, мир";
        match parse_mutf8(string.as_bytes()) {
            Ok(Cow::Borrowed(text)) => assert_eq!(text, string),
            other => panic!(other),
        }
    }

    #[test]
    fn test_mutf8_supplementary() {
        let string = &[0x41, 0xed, 0xa0, 0xb4, 0xed, 0xbc, 0xb9];
        match parse_mutf8(string) {
            Ok(Cow::Owned(text)) => assert_eq!(text, "A𝌹"),
            other => panic!(other),
        }
    }

    #[test]
    fn test_mutf8_overlong_nul() {
        let string = b"foo\xc0\x80bar\xc0\x80baz";
        assert_eq!(
            Ok(Cow::Owned(String::from("foo\0bar\0baz"))),
            parse_mutf8(string)
        );
    }

    #[test]
    fn test_mutf8_invalid_nul() {
        let string = b"foo\0bar";
        assert_eq!(
            parse_mutf8(string),
            Err(ClassError::InvalidModifiedUtf8Byte(3, 0))
        )
    }
}
