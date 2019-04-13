mod scanner;

use std::fs;
use scanner::Scanner;
use scanner::Token;

fn main() {
    let src = fs::read_to_string("test.txt").expect("something went wrong");
    let mut s = Scanner::from_string(&src);

    loop {
        let ts = s.scan();
        match ts.tok {
            Token::Eof => {
                println!("breaking on Eof");
                break;
            },
            Token::Err => {
                println!("breaking on Err");
                break;
            },
            _ => (),
        }
    }
}
