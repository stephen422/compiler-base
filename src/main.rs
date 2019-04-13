mod lexer;

use lexer::Lexer;

fn main() {
    let l = Lexer::from_file("test.txt");
    println!("{}", l.src);
}
