use std::{
    collections::HashSet,
    fs::{self, File},
    io::Write,
};

use compiler::compile;
use cpu::process;
use irstring::stringify;
use lexer::tokenize;
use parser::parse;
use ssa::to_ssa;

// Write lexer
// Write parser
// Write CPU that can handle ASTs
// Show inner workings and how ideas work there

pub mod compiler;
pub mod cpu;
pub mod irstring;
pub mod lexer;
pub mod parser;
pub mod ssa;

fn main() {
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

    let ast = match parse(tokens) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    let mut s = "".to_string();
    for i in 0..ast.len() {
        s.push_str(&format!("{}) {:?}\n", i + 1, ast[i]));
    }
    let mut file = fs::File::create("src/ast_code.txt").unwrap();
    let _ = file.write_all(s.as_bytes());

    // let mut vars = HashSet::new();
    // let ssa = to_ssa(&mut ast, &mut vars);

    // println!("---------------------------------------------");

    // for i in 0..ssa.len() {
    //     println!("{}) {:?}", i + 1, ssa[i]);
    // }

    // Build IR Compiler

    let strings_out = match stringify(&ast) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    let mut s = "".to_string();
    for i in 0..strings_out.len() {
        s.push_str(&format!("{:0>2}) {}\n", i + 1, strings_out[i]));
    }
    let mut file = fs::File::create("src/string_code.txt").unwrap();
    let _ = file.write_all(s.as_bytes());

    let compiled_code = compile(&ast);
    let mut s = "".to_string();
    for i in 0..compiled_code.len() {
        // println!("{:0>2} {:?}", i + 1, compiled_code[i]);
        s.push_str(&format!("{:0>2} {:?}\n", i + 1, compiled_code[i]));
    }

    let mut file = fs::File::create("src/compiled_code.txt").unwrap();
    let _ = file.write_all(s.as_bytes());

    let cpustate = process(compiled_code);

    println!("Instruction Count : {}", cpustate.inst_executed);
    println!("Labels : {:?}", cpustate.labels);
    println!("Arrays : {:?}", cpustate.arrays);
    println!("Variables : {:?}", cpustate.vars);
}
