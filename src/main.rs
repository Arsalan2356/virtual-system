use lexer::tokenize;
use parser::parse;
use ssa::ssa;

// Write lexer
// Write parser
// Write CPU that can handle ASTs
// Show inner workings and how ideas work there

pub mod lexer;
pub mod parser;
pub mod ssa;

fn main() {
    let s = "
        v = 24;
        v = v + 12;
        b = 295;
        c = 24.0;
        c = c + 142.143;
        fun f(x y) {
            x = x + 12;
            y = y + 24;
        }
        f(v b);
        if (v > 20) {
            v = v - 53;
            b = 24;
            z(v b);
        }
        loop {
            v = 1 + v;
            break (v > 20);
        }
        if (v > 20) {
            v = 4 * v;
            fun z(q v) {
                q = q - 12;
                v = v * 2;
            }
        } else {
            v = 3 * v;
            z(v b);
        }
        q = {
            1.0 2.0 3.0 4.0,
            1.0 4.0 9.0 16.0,
            1.0 8.0 27.0 64.0,
        };
        q = {
            0.012 0.0013 0.0142 0.0014,
            0.0131 0.1453 0.9052 0.2852,
            0.3637 0.6375 0.6847 0.2385,
        };

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

    println!("---------------------------------------------");

    for i in 0..ast.len() {
        println!("{}) {:?}", i + 1, ast[i]);
    }

    // Build IR Compiler

    let ssa_out = match ssa(ast) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    println!("---------------------------------------------");

    for i in 0..ssa_out.len() {
        print!("{}) {}", i + 1, ssa_out[i]);
    }
}
