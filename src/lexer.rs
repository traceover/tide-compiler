use derive_more::{IsVariant, Unwrap};
use num_bigint::BigInt;
use std::path::PathBuf;
use std::{fmt, fs, io};

/// A custom location type to represent the location of tokens or errors.
/// This enum has two variants: `File` and `Str`.
/// `File` is used when the source is from a file and includes the file path, row, and column.
/// `Str` is used for string inputs and includes the column and the line as a vector of characters.
#[derive(Debug, Clone)]
pub enum Loc {
    File {
        path: String,
        row: usize,
        col: usize,
    },
    Str {
        col: usize,
        line: Vec<char>,
    },
}

/// Formats the location as it would appear in many other programming language compilers,
/// with platform-specific formatting to print backslashes on Windows.
/// Special handling for the `Str` variant is not handled here to print a proper backtrace
/// that will underline the erroneous code, instead, the row will be omitted and only a
/// column number will be displayed.
impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Loc::File { path, row, col } => {
                write!(
                    f,
                    "{path}:{row}:{col}",
                    path = PathBuf::from(&path).display(),
                    col = col + 1
                )
            }
            Loc::Str { col, line: _ } => {
                write!(f, "(from a string):{col}", col = col + 1)
            }
        }
    }
}

pub struct Lexer {
    chars: Vec<char>,
    file_path: Option<String>,
    lnum: usize,
    bol: usize,
    cnum: usize,
    error: Option<LexError>,
}

impl Lexer {
    /// Constructs a new `Lexer` instance from the given character vector and optional file path.
    /// Initializes line number, beginning of line, and current character number to zero.
    pub fn new(chars: Vec<char>, file_path: Option<String>) -> Self {
        Self {
            chars,
            file_path,
            lnum: 0,
            bol: 0,
            cnum: 0,
            error: None,
        }
    }

    pub fn from_file_path(file_path: String) -> Result<Self, io::Error> {
        let chars: Vec<_> = fs::read_to_string(&file_path)?.chars().collect();
        Ok(Lexer::new(chars, Some(file_path)))
    }

    /// Retrieves the current line being analyzed as a vector of characters.
    pub fn current_line(&self) -> Vec<char> {
        let mut eol = self.bol;
        while eol < self.chars.len() && self.chars[eol] != '\n' {
            eol += 1;
        }
        return self.chars[self.bol..eol].to_vec();
    }

    /// Provides the current location (`Loc`) in the source code, either in a file or a string.
    pub fn loc(&self) -> Loc {
        match &self.file_path {
            Some(file_path) => Loc::File {
                path: file_path.clone(),
                row: self.lnum + 1,
                col: self.cnum - self.bol,
            },
            None => Loc::Str {
                col: self.cnum - self.bol,
                line: self.current_line(),
            },
        }
    }

    #[inline]
    fn drop_char_if<F>(&mut self, predicate: F) -> Option<char>
    where
        F: FnOnce(char) -> bool,
    {
        self.chars.get(self.cnum).copied().and_then(|ch| {
            if predicate(ch) {
                self.drop_char()
            } else {
                None
            }
        })
    }

    #[inline]
    fn drop_char_by(&mut self, set: &str) -> Option<char> {
        self.drop_char_if(|ch| set.contains(ch))
    }

    #[inline]
    fn drop_char(&mut self) -> Option<char> {
        self.chars.get(self.cnum).copied().map(|ch| {
            self.cnum += 1;
            if ch == '\n' {
                self.bol = self.cnum;
                self.lnum += 1;
            }
            ch
        })
    }

    #[inline]
    fn drop_line(&mut self) {
        while let Some(x) = self.drop_char() {
            if x == '\n' {
                return;
            }
        }
    }

    #[inline]
    fn switch2(&mut self, tok0: Token, tok1: Token) -> Token {
        if self.drop_char_if(|ch| ch == '=').is_some() {
            tok1
        } else {
            tok0
        }
    }

    #[inline]
    fn switch3(&mut self, tok0: Token, tok1: Token, ch: char, tok2: Token) -> Token {
        match self.drop_char_if(|x| x == '=' || x == ch) {
            Some('=') => tok1,
            Some(_) => tok2,
            None => tok0,
        }
    }

    #[inline]
    fn trim_whitespace(&mut self) {
        while let Some(x) = self.chars.get(self.cnum) {
            if x.is_whitespace() {
                self.drop_char();
            } else {
                break;
            }
        }
    }

    fn chop_tokens_from_chars(&mut self) -> Option<Token> {
        'again: loop {
            self.trim_whitespace();

            use Token::*;
            return Some(match self.drop_char()? {
                '(' => OpenParen,
                ')' => CloseParen,
                '{' => OpenBrace,
                '}' => CloseBrace,
                '[' => OpenBracket,
                ']' => CloseBracket,
                ',' => Comma,
                '.' => Period,
                ':' => Colon,
                ';' => Semicolon,
                '=' => self.switch2(Equals, EqualsEquals),
                '!' => self.switch2(Bang, BangEquals),
                '+' => self.switch2(Plus, PlusEquals),
                '-' => self.switch3(Minus, MinusEquals, '>', Arrow),
                '*' => self.switch2(Asterisk, AsteriskEquals),
                '/' => match self.drop_char_by("=/") {
                    Some('=') => SlashEquals,
                    Some('/') => {
                        self.drop_line();
                        continue 'again;
                    }
                    _ => Slash,
                },
                '%' => self.switch2(Percent, PercentEquals),
                '&' => self.switch3(Ampersand, AmpersandEquals, '&', AmpersandAmpersand),
                '|' => self.switch3(Pipe, PipeEquals, '|', PipePipe),
                '^' => self.switch2(Caret, CaretEquals),
                '"' => {
                    let start = self.loc();
                    let mut text = String::new();
                    while let Some(x) = self.drop_char_if(|x| x != '"') {
                        text.push(x);
                    }
                    if let Some(_) = self.drop_char_if(|x| x == '"') {
                        StrLit(text)
                    } else {
                        self.error = Some(LexError::UnterminatedStrLit { start });
                        return None;
                    }
                }
                x => {
                    let loc = self.loc();
                    if is_ident_char(x) {
                        let mut text = x.to_string();
                        while let Some(x) = self.drop_char_if(is_ident_char) {
                            text.push(x);
                        }

                        if let Some(kind) = keyword_by_name(&text) {
                            kind
                        } else if text.starts_with(|c: char| c.is_ascii_digit()) {
                            match text.parse::<BigInt>() {
                                Ok(x) => IntLit(x),
                                Err(_) => {
                                    self.error = Some(LexError::InvalidIntLit { text, loc });
                                    return None;
                                }
                            }
                        } else {
                            Ident(text)
                        }
                    } else {
                        self.error = Some(LexError::InvalidChar { ch: x, loc });
                        return None;
                    }
                }
            });
        }
    }
}

#[derive(Debug, Clone, PartialEq, IsVariant, Unwrap)]
pub enum Token {
    // Literals / multi-char tokens
    Ident(String),
    StrLit(String),
    IntLit(BigInt),
    FloatLit(f64),

    // Punctuation tokens
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Period,
    Colon,
    Semicolon,
    Arrow,

    // Operator tokens
    Equals,
    EqualsEquals,
    Bang,
    BangEquals,
    Plus,
    PlusEquals,
    Minus,
    MinusEquals,
    Asterisk,
    AsteriskEquals,
    Slash,
    SlashEquals,
    Percent,
    PercentEquals,
    Ampersand,
    AmpersandAmpersand,
    AmpersandEquals,
    Pipe,
    PipePipe,
    PipeEquals,
    Caret,
    CaretEquals,

    // Keyword tokens
    If,
    Else,
    Then,
    For,
    While,
    Return,

    // Not an actual token, but used in the parser to detect the end
    // without having to bounds check
    EndOfFile,
}

pub enum LexError {
    UnterminatedStrLit { start: Loc },
    InvalidChar { loc: Loc, ch: char },
    InvalidIntLit { loc: Loc, text: String },
}

/// Implementing `Iterator` for `Lexer`, where each iteration returns a `Token` and its `Loc`.
impl Iterator for Lexer {
    type Item = (Token, Loc);

    /// Returns the next `Token` and its `Loc` in the source code, or `None` if the end is reached.
    fn next(&mut self) -> Option<Self::Item> {
        self.trim_whitespace(); // So that the location is correct
        let loc = self.loc();
        Some((self.chop_tokens_from_chars()?, loc))
    }
}

fn is_ident_char(x: char) -> bool {
    let extra_chars = "_";
    x.is_alphanumeric() || extra_chars.contains(x)
}

fn keyword_by_name(s: &str) -> Option<Token> {
    match s {
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "then" => Some(Token::Then),
        "for" => Some(Token::For),
        "while" => Some(Token::While),
        "return" => Some(Token::Return),
        _ => None,
    }
}
