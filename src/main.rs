mod scanner;

use scanner::Scanner;
use scanner::Token;
use std::fs;

fn main() {
    let src = fs::read_to_string("test.txt").expect("could not open file");
    let mut s = Scanner::from_string(&src);

    loop {
        let ts = s.scan();
        match ts.tok {
            Token::Ident(name) => println!("{}:[{}]", ts.pos, name.str),
            Token::KwLet => println!("{}:let", ts.pos),
            Token::Eof => {
                println!("{}:EOF", ts.pos);
                break;
            }
            Token::Err => {
                println!("breaking on Err");
                break;
            }
            _ => (),
        }
    }
}
