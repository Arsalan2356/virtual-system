use std::{
    fs::{self},
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
    let s =
        fs::read_to_string("src/neural_net.txt").expect("Expected a text file called neural_net");

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

    let cpustate = process(&compiled_code, false, false, false, false, 10, 1);
    println!(
        "Non-Optimized Instruction Count : {}",
        cpustate.inst_executed
    );

    let cpustate = process(&compiled_code, true, false, false, false, 10, 1);
    println!("FMA Instruction Count : {}", cpustate.inst_executed);

    let cpustate = process(&compiled_code, true, true, false, false, 10, 1);
    println!("FMA + FPU Instruction Count : {}", cpustate.inst_executed);

    let cpustate = process(&compiled_code, true, true, true, false, 10, 1);
    println!(
        "FMA + FPU + Branch Instruction Count : {}",
        cpustate.inst_executed
    );

    let cpustate = process(&compiled_code, true, true, true, true, 10, 1);
    println!(
        "FMA + FPU + Branch + Inline Instruction Count : {}",
        cpustate.inst_executed
    );

    let cpustate = process(&compiled_code, true, true, true, true, 1, 1);
    println!(
        "Only L1 Cache Instruction Count : {}",
        cpustate.inst_executed
    );

    let cpustate = process(&compiled_code, true, true, true, true, 1, 8);
    println!("Vectorized Instruction Count : {}", cpustate.inst_executed);
    // println!("Array Values : {:?}", cpustate.arr_vals);
}
