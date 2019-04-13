mod lexer;

use std::fs;
use lexer::Lexer;

fn main() {
    let src = fs::read_to_string("test.txt").expect("something went wrong");
    let mut l = Lexer::from_string(&src);
    l.lex();
}
