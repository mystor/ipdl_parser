//! Fairly minimal recursive-descent parser helper functions and types.

#[derive(Debug)]
pub struct Error {
    pub expected: Vec<&'static str>,
    pub offset: usize,
    pub(crate) fatal: bool,
}

/// Attempts to run each expression in order, recovering from any non-fatal
/// errors and attempting the next option.
macro_rules! any {
    ($($e:expr => |$x:ident| $f:expr),+ $(,)*) => {
        loop {
            let mut err = Error {
                expected: Vec::new(),
                offset: 0,
                fatal: false,
            };
            $(match $e {
                Ok((i, $x)) => break Ok((i, $f)),
                Err(e) => if e.fatal {
                    break Err(e);
                } else {
                    err.expected.extend(e.expected);
                    err.offset = e.offset;
                }
            })+
            break Err(err);
        }
    };

    ($($e:expr => $f:expr),+ $(,)*) => {
        any!($($e => |_x| $f),+);
    }
}

/// Attempts to repeatedly run the expression, stopping on a non-fatal error,
/// and directly returning any fatal error.
macro_rules! drive {
    ($i:ident, $e:expr) => {
        let mut $i = $i;
        loop {
            match $e {
                Ok((j, _)) => $i = j,
                Err(e) => if e.fatal {
                    return Err(e);
                } else {
                    break;
                }
            }
        }
    }
}

/// This trait is implemented on all PResult-like types. It provides the nf()
/// method which marks a particular error as non-fatal, allowing recovery during
/// any!() or drive!().
pub(crate) trait PResultLike {
    fn nf(self) -> Self;
}
impl<T> PResultLike for Result<T, Error> {
    fn nf(mut self) -> Self {
        match self {
            Err(ref mut e) => e.fatal = false,
            _ => {}
        }
        self
    }
}

/// The type of error used by internal parsers
pub(crate) type PResult<'a, T> = Result<(In<'a>, T), Error>;

/// This datastructure is used as the cursor type into the input source data. It
/// holds the full source string, and the current offset.
#[derive(Copy, Clone)]
pub(crate) struct In<'a> {
    src: &'a str,
    offset: usize,
}
impl<'a> In<'a> {
    pub(crate) fn new(s: &'a str) -> Self  {
        In { src: s, offset: 0 }
    }

    /// The remaining string in the source file.
    pub(crate) fn rest(&self) -> &'a str {
        &self.src[self.offset..]
    }

    /// Move the cursor forward by `n` bytes.
    pub(crate) fn advance(&self, n: usize) -> Self {
        let offset = self.offset.checked_add(n).unwrap();
        assert!(offset <= self.src.len());
        In { src: self.src, offset }
    }

    /// Produce a new fatal error result expecting the given tokens. This error
    /// can be made non-fatal by calling the .nf() method.
    pub(crate) fn expected<T>(
        &self,
        expected: Vec<&'static str>,
    ) -> Result<T, Error> {
        Err(Error {
            expected: expected,
            offset: self.offset,
            fatal: true,
        })
    }
}

/// Repeatedly run f, collecting results into a vec. Returns an error if a fatal
/// error is produced while parsing.
pub(crate) fn many<F, R>(i: In, mut f: F) -> PResult<Vec<R>>
where
    F: FnMut(In) -> PResult<R>,
{
    let mut v = Vec::new();
    drive!(i, match f(i) {
        Ok((i, x)) => {
            v.push(x);
            Ok((i, ()))
        }
        Err(e) => Err(e),
    });
    Ok((i, v))
}

/// Repeatedly run f, followed by parsing the seperator sep. Returns an error if
/// a fatal error is produced while parsing.
pub(crate) fn sep<'a, F, R>(
    i: In<'a>,
    mut f: F,
    sep: &'static str,
) -> PResult<'a, Vec<R>>
where
    F: FnMut(In<'a>) -> PResult<'a, R>,
{
    let mut v = Vec::new();
    drive!(i, match f(i) {
        Ok((i, x)) => {
            v.push(x);
            match punct(i, sep) {
                Ok(o) => Ok(o),
                Err(_) => return Ok((i, v)),
            }
        }
        Err(e) => Err(e),
    });
    Ok((i, v))
}

/// Skip any leading whitespace, including comments
pub(crate) fn skip_ws(mut i: In) -> Result<In, Error> {
    loop {
        if i.rest().is_empty() {
            break;
        }

        let c = i.rest().chars().next().unwrap();
        if c.is_whitespace() {
            i = i.advance(c.len_utf8());
            continue;
        }

        // Line comments
        if i.rest().starts_with("//") {
            let x = i.rest().find('\n').unwrap_or(i.rest().len());
            i = i.advance(x);
            continue;
        }

        // Block comments
        if i.rest().starts_with("/*") {
            if let Some(x) = i.rest().find("*/") {
                i = i.advance(x + 2);
                continue;
            }
            return i.advance(i.rest().len()).expected(vec!["*/"]);
        }
        break;
    }

    Ok(i)
}

/// Read an identifier as a string.
pub(crate) fn ident(i: In) -> PResult<String> {
    let i = skip_ws(i)?;
    let end = i.rest()
        .char_indices()
        .skip_while(|&(idx, c)| match c {
            '_' | 'a'...'z' | 'A'...'Z' => true,
            '0'...'9' if idx != 0 => true,
            _ => false,
        })
        .next()
        .map(|x| x.0)
        .unwrap_or(i.rest().len());

    if end == 0 {
        return i.expected(vec!["identifier"]);
    }

    Ok((i.advance(end), i.rest()[..end].to_owned()))
}

/// Parse a specific keyword.
pub(crate) fn kw<'a>(i: In<'a>, kw: &'static str) -> PResult<'a, ()> {
    let (j, id) = ident(i)?;
    if id == kw {
        Ok((j, ()))
    } else {
        i.expected(vec![kw])
    }
}

/// Parse punctuation.
pub(crate) fn punct<'a>(i: In<'a>, p: &'static str) -> PResult<'a, ()> {
    let i = skip_ws(i)?;
    if i.rest().starts_with(p) {
        Ok((i.advance(p.len()), ()))
    } else {
        i.expected(vec![p])
    }
}

/// Try to parse the inner value, and return Some() if it succeeded, None if it
/// failed non-fatally, and an error if it failed fatally.
pub(crate) fn maybe<'a, T>(
    i: In<'a>,
    r: PResult<'a, T>
) -> PResult<'a, Option<T>> {
    match r {
        Ok((i, x)) => Ok((i, Some(x))),
        Err(e) => if e.fatal {
            Err(e)
        } else {
            Ok((i, None))
        }
    }
}

/// Parse a string literal.
pub(crate) fn string(i: In) -> PResult<String> {
    let mut s = String::new();
    let (i, _) = punct(i, "\"")?;
    let mut chars = i.rest().char_indices().peekable();
    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => return Ok((i.advance(byte_offset + 1), s)),
            '\\' => match chars.next() {
                Some((_, 'n')) => s.push('\n'),
                Some((_, 'r')) => s.push('\r'),
                Some((_, 't')) => s.push('\t'),
                Some((_, '\\')) => s.push('\\'),
                Some((_, '\'')) => s.push('\''),
                Some((_, '"')) => s.push('"'),
                Some((_, '0')) => s.push('\0'),
                _ => return i.advance(byte_offset).expected(vec![
                    "\\n", "\\r", "\\t", "\\\\", "\\'", "\\\"", "\\0"
                ]),
            }
            x => s.push(x),
        }
    }
    i.expected(vec!["\""])
}

/// Produce a successful value.
pub(crate) fn epsilon(i: In) -> PResult<()> { Ok((i, ())) }
