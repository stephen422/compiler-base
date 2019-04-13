// TODO
pub struct Name {}

pub enum Token {
    Ident(Name),
    Eof,
}

pub struct Lexer<'a> {
    ch: char,
    pos: usize,
    iter: std::str::CharIndices<'a>,
}

impl<'a> Lexer<'a> {
    pub fn from_string(src: &str) -> Lexer {
        let mut l = Lexer {
            ch: '\0',
            pos: 0,
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
                    // TODO
                }
            }
            None => {
                // self.pos = self.src.len();
                self.ch = '\0'; // EOF
            }
        }
        println!("bump: {}", self.ch);
    }

    pub fn lex(&mut self) -> Token {
        self.bump();
        Token::Eof
    }
}
