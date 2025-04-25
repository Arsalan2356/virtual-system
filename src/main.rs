use std::{collections::HashSet, fs};

use irstring::stringify;
use lexer::tokenize;
use parser::parse;
use ssa::to_ssa;

// Write lexer
// Write parser
// Write CPU that can handle ASTs
// Show inner workings and how ideas work there

pub mod irstring;
pub mod lexer;
pub mod parser;
pub mod ssa;

fn main() {
    // let s = "
    //     v;
    //     v = 24;
    //     v = v + 12;
    //     v = v + 32;
    //     b = 295;
    //     c = 24.0;
    //     c = c + 142.143;
    //     fun f(x y) {
    //         x = x + 12;
    //         y = y + 24;
    //     }
    //     f(v b);
    //     if (v > 20) {
    //         v = v - 53;
    //         b = 24;
    //         z(v b);
    //     }
    //     loop {
    //         v = 1 + v;
    //         break (v > 20);
    //     }
    //     if (v > 20) {
    //         v = 4 * v;
    //         fun z(q v) {
    //             q = q - 12;
    //             v = v * 2;
    //         }
    //     } else {
    //         v = 3 * v;
    //         z(v b);
    //     }
    //     q = {
    //         1.0 2.0 3.0 4.0,
    //         1.0 4.0 9.0 16.0,
    //         1.0 8.0 27.0 64.0,
    //     };
    //     q = {
    //         0.012 0.0013 0.0142 0.0014,
    //         0.0131 0.1453 0.9052 0.2852,
    //         0.3637 0.6375 0.6847 0.2385,
    //     };

    //     ";

    let s = "
        x = 5;
        x = x - 3;
        y = 4;
        y = y * 2;
        if (x < 3) {
            y = y * x;
           	y = y - 2;
        }
        ";

    let s = fs::read_to_string("src/neural_net_copy.txt")
        .expect("Expected a text file called neural_net");

    let tokens = match tokenize(s.to_string().replace("\n", "").trim_ascii().to_string()) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    // println!("{:?}", tokens);
    println!("{} tokens", tokens.len());

    let mut ast = match parse(tokens) {
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

    // let mut vars = HashSet::new();
    // let ssa = to_ssa(&mut ast, &mut vars);

    // println!("---------------------------------------------");

    // for i in 0..ssa.len() {
    //     println!("{}) {:?}", i + 1, ssa[i]);
    // }

    // Build IR Compiler

    let strings_out = match stringify(ast) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    println!("---------------------------------------------");

    for i in 0..strings_out.len() {
        print!("{:0>2}) {}", i + 1, strings_out[i]);
    }
}
