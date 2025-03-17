use lexer::tokenize;
use parser::parse;

// Write lexer
// Write parser
// Write CPU that can handle ASTs
// Show inner workings and how ideas work there

pub mod lexer;
pub mod parser;

fn main() {
    // let s = "
    //     v = 24;
    //     v = v + 12;
    //     if (v > 20) {
    //         v = 2;
    //     }
    //     if (v > 30) {
    //         v = 5;
    //     }
    //     else {
    //         v = 10;
    //     }
    //     fun f(x) {
    //         x = x * 2;
    //         return x;
    //     }
    //     loop {
    //         v = v + 10;
    //         break (v > 50);
    //     }
    //     f(x);
    //     ";

    let s = "
        v = 24;
        v = v + 12;
        if (v > 20) {
            v = 2;
        }
        ";

    let tokens = match tokenize(s.to_string().replace("\n", "").trim_ascii().to_string()) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    println!("{:?}", tokens);

    let ast = match parse(tokens) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    println!("{:?}", ast);
}
