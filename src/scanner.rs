use std::collections::HashMap;
use std::iter::Peekable;
use std::str::CharIndices;

// TODO
#[derive(Clone)]
pub struct Name {
    pub str: String,
}

#[derive(Clone)]
pub enum Token<'a> {
    Ident(&'a Name),
    Number,
    String,
    Comment,
    Newline,
    Arrow,
    Quote,
    DQuote,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Dot,
    Comma,
    Colon,
    Equals,
    Plus,
    Star,
    Ampersand,
    Slash,
    KwFn,
    KwLet,
    KwVar,
    KwReturn,
    KwIf,
    KwElse,
    KwFor,
    KwInt,
    Eof,
    Err,
}

#[derive(Clone)]
pub struct TokenAndPos<'a> {
    pub tok: Token<'a>,
    pub pos: usize,
}

pub struct Scanner<'a> {
    ch: char,
    pos: usize,   // current lookahead position
    start: usize, // start of the currently scanned token
    line_offs: Vec<usize>,
    iter: Peekable<CharIndices<'a>>,
    name_table: HashMap<String, Name>,
}

// NOTE: Longer strings should come first, as this map is subject to front-to-rear linear search.
// The search may end prematurely if a shorter substring matches first.
const TOKEN_STR_MAP: &[(&str, Token)] = &[
    ("return", Token::KwReturn),
    ("else", Token::KwElse),
    ("let", Token::KwLet),
    ("var", Token::KwVar),
    ("for", Token::KwFor),
    ("int", Token::KwInt),
    ("fn", Token::KwFn),
    ("if", Token::KwIf),
    ("->", Token::Arrow),
    ("\"", Token::DQuote),
    ("\n", Token::Newline),
    ("'", Token::Quote),
    ("(", Token::Lparen),
    (")", Token::Rparen),
    ("{", Token::Lbrace),
    ("}", Token::Rbrace),
    (".", Token::Dot),
    (",", Token::Comma),
    (":", Token::Colon),
    ("=", Token::Equals),
    ("+", Token::Plus),
    ("*", Token::Star),
    ("&", Token::Ampersand),
    ("/", Token::Slash),
];

impl<'a> Scanner<'a> {
    pub fn from_string(src: &str) -> Scanner {
        let mut l = Scanner {
            ch: '\0',
            pos: 0,
            start: 0,
            line_offs: Vec::new(),
            iter: src.char_indices().peekable(),
            name_table: HashMap::new(),
        };
        l.sync_cache();
        l
    }

    fn sync_cache(&mut self) {
        match self.iter.peek() {
            Some(&(pos, ch)) => {
                self.pos = pos;
                self.ch = ch;
                if self.ch == '\n' {
                    self.line_offs.push(self.pos);
                }
            }
            None => {
                self.ch = '\0';
            }
        }
    }

    fn bump(&mut self) {
        self.iter.next();
        self.sync_cache();
    }

    fn is_end(&self) -> bool {
        self.ch == '\0'
    }

    fn skip_while(&mut self, f: &Fn(char) -> bool) {
        while !self.is_end() && f(self.ch) {
            self.bump();
        }
    }

    fn take_while(&mut self, f: &Fn(char) -> bool) -> String {
        let mut s = String::new();
        while !self.is_end() && f(self.ch) {
            s.push(self.ch);
            self.bump();
        }
        s
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(&|ch: char| ch.is_whitespace());
    }

    fn scan_ident(&mut self) -> TokenAndPos {
        // First try to match with a keyword.

        for (str, tok) in TOKEN_STR_MAP {
            let excerpt = self.iter.clone().map(|(_, ch)| ch).take(str.len());

            if str.chars().eq(excerpt) {
                for _ in 0..str.len() {
                    self.bump();
                }

                return TokenAndPos {
                    tok: tok.clone(),
                    pos: self.start,
                };
            }
        }

        // if self.scan_symbol().is_some() {
        //     return match self.scan_symbol() {
        //         Some(tp) => return tp,
        //         None => TokenAndPos {
        //             tok: Token::Err,
        //             pos: 0,
        //         }
        //     };
        // }

        // TODO Big hack!
        let s = self.take_while(&|ch: char| ch.is_alphanumeric() || ch == '_');
        let key = s.clone();
        let key2 = key.clone();
        let name = Name { str: s };
        self.name_table.insert(key, name);
        let n = self.name_table.get(&key2).unwrap();
        TokenAndPos {
            tok: Token::Ident(n),
            pos: self.start,
        }
    }

    fn scan_number(&mut self) -> TokenAndPos {
        self.skip_while(&|ch: char| ch.is_numeric());
        // TODO: take the numeric value
        TokenAndPos {
            tok: Token::Number,
            pos: self.start,
        }
    }

    fn scan_string(&mut self) -> TokenAndPos {
        self.bump(); // opening "

        while !self.is_end() {
            self.skip_while(&|ch: char| ch != '\\' && ch != '"');

            match self.ch {
                '"' => {
                    self.bump(); // closing "
                    break;
                }
                '\\' => {
                    // escaped character ("\x")
                    self.bump();
                    self.bump();
                }
                _ => unreachable!(),
            }
        }

        TokenAndPos {
            tok: Token::String,
            pos: self.start,
        }
    }

    fn scan_symbol(&mut self) -> Option<TokenAndPos> {
        for (str, tok) in TOKEN_STR_MAP {
            let excerpt = self.iter.clone().map(|(_, ch)| ch).take(str.len());

            if str.chars().eq(excerpt) {
                for _ in 0..str.len() {
                    self.bump();
                }

                return Some(TokenAndPos {
                    tok: tok.clone(),
                    pos: self.start,
                });
            }
        }

        None
    }

    fn scan_comment_or_slash(&mut self) -> TokenAndPos {
        self.bump();
        match self.ch {
            '/' => {
                self.skip_while(&|ch: char| ch != '\n');
                return TokenAndPos {
                    tok: Token::Comment,
                    pos: self.start,
                };
            }
            _ => {
                return TokenAndPos {
                    tok: Token::Slash,
                    pos: self.start,
                };
            }
        }
    }

    pub fn scan(&mut self) -> TokenAndPos {
        self.skip_whitespace();
        self.start = self.pos;

        if self.is_end() {
            return TokenAndPos {
                tok: Token::Eof,
                pos: self.start,
            };
        }

        let start = self.start;

        match self.ch {
            '"' => self.scan_string(),
            '/' => self.scan_comment_or_slash(),
            ch => {
                if ch.is_alphabetic() || ch == '_' {
                    self.scan_ident()
                } else if ch.is_numeric() {
                    self.scan_number()
                } else {
                    match self.scan_symbol() {
                        Some(tp) => tp,
                        None => TokenAndPos {
                            tok: Token::Err,
                            // Returning self.start here triggers borrow checker error, due to the
                            // current limitation of the Rust NLL.  Instead, we save self.start to
                            // a local variable beforehand and return it.
                            pos: start,
                        },
                    }
                }
            }
        }
    }
}
