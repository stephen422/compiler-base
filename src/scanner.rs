// TODO
pub struct Name {
    str: String,
}

pub enum Token {
    Ident(Name),
    Eof,
    Err,
}

pub struct TokenAndPos {
    pub tok: Token,
    pos: usize,
}

pub struct Scanner<'a> {
    ch: char,
    pos: usize,   // current lookahead position
    pivot: usize, // start of the currently scanned token
    line_offs: Vec<usize>,
    iter: std::str::CharIndices<'a>,
}

impl<'a> Scanner<'a> {
    pub fn from_string(src: &str) -> Scanner {
        let mut l = Scanner {
            ch: '\0',
            pos: 0,
            pivot: 0,
            line_offs: Vec::new(),
            iter: src.char_indices(),
        };
        l.bump();
        l
    }

    fn bump(&mut self) {
        let ci = self.iter.next();
        match ci {
            Some((pos, ch)) => {
                self.pos = pos;
                self.ch = ch;
                if self.ch == '\n' {
                    self.line_offs.push(self.pos);
                }
            }
            None => {
                // self.pos = self.src.len();
                self.ch = '\0'; // EOF
            }
        }
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
        println!("scan_ident: [{}]", s);
        TokenAndPos {tok: Token::Ident(Name {str: s}), pos: self.pos}
    }

    pub fn scan(&mut self) -> TokenAndPos {
        self.skip_whitespace();

        if self.is_end() {
            return TokenAndPos {
                tok: Token::Eof,
                pos: self.pos,
            };
        }

        match self.ch {
            '"' => TokenAndPos {tok: Token::Err, pos: self.pos },
            '/' => TokenAndPos {tok: Token::Err, pos: self.pos },
            ch => {
                if ch.is_alphabetic() || ch == '_' {
                    self.scan_ident()
                } else {
                    TokenAndPos {tok: Token::Err, pos: self.pos }
                }
            }
        }
    }
}
