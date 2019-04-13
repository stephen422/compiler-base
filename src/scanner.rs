use std::iter::Peekable;
use std::str::CharIndices;

// TODO
#[derive(Clone)]
pub struct Name {
    pub str: String,
}

#[derive(Clone)]
pub enum Token {
    Ident(Name),
    Number,
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
    Eof,
    Err,
}

pub struct TokenAndPos {
    pub tok: Token,
    pub pos: usize,
}

pub struct Scanner<'a> {
    ch: char,
    pos: usize,   // current lookahead position
    start: usize, // start of the currently scanned token
    line_offs: Vec<usize>,
    iter: Peekable<CharIndices<'a>>,
}

// NOTE: Longer strings should come first, as this map is subject to front-to-rear linear search,
// and the search may terminate prematurely if a shorter substring matches the given string first.
const TOKEN_STR_MAP: &[(&str, Token)] = &[
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
        };
        l.cache();
        l
    }

    fn cache(&mut self) {
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
        self.cache();
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
        let mut s = "".to_string();
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
        let s = self.take_while(&|ch: char| ch.is_alphanumeric() || ch == '_');
        TokenAndPos {
            tok: Token::Ident(Name { str: s }),
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

    fn scan_symbol(&mut self) -> TokenAndPos {
        // Should be careful about comparing against multi-char symbols as the source string may
        // terminate early.

        'cand: for (s, tok) in TOKEN_STR_MAP {
            let mut cand_iter = s.chars();
            let mut iter = self.iter.clone();

            loop {
                let ch_cand: char;
                match cand_iter.next() {
                    Some(ch) => ch_cand = ch,
                    None => {
                        // Termination of candidate string alone is sufficient for a complete
                        // match.  The longest-to-front rule for TOKEN_STR_MAP guarantees that no
                        // other candidate can match the source string with more characters.
                        self.iter = iter;
                        self.cache();
                        return TokenAndPos {tok: tok.clone(), pos: self.start};
                    }
                }

                let ch_src: char;
                match iter.next() {
                    Some((_, ch)) => ch_src = ch,
                    None => {
                        // Termination of the source string, without the candidate string being
                        // terminated, means a failed match.
                        continue 'cand;
                    }
                }

                if ch_src != ch_cand {
                    continue 'cand;
                }
            }
        }

        println!("unknown symbol: [{}]", self.ch);
        TokenAndPos {
            tok: Token::Err,
            pos: self.start,
        }
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

        match self.ch {
            '"' => TokenAndPos {
                tok: Token::Err,
                pos: self.start,
            },
            '/' => self.scan_comment_or_slash(),
            ch => {
                if ch.is_alphabetic() || ch == '_' {
                    self.scan_ident()
                } else if ch.is_numeric() {
                    self.scan_number()
                } else {
                    self.scan_symbol()
                }
            }
        }
    }
}
