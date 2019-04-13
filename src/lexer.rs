use std::fs;

// TODO
pub struct Name {}

pub enum Token {
    Ident(Name),
    Eof,
}

pub struct Lexer {
    pos: i64,
    pub src: String,
}

impl Lexer {
    pub fn from_file(path: &str) -> Lexer {
        let src = fs::read_to_string(path).expect("something went wrong");
        Lexer {
            pos: 0,
            src: src,
        }
    }

    fn bump() {}

    pub fn lex() -> Token {
        Token::Eof
    }
}
