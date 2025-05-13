// reg 0 is always 0
// reg 1 is always 1
// for any variable x, if x is stored in reg i, it will always be in reg i
// for the lifetime of the program
// all variables are predefined at the start of a program
// they all have fixed sizes and thus can be stored in a register
// arrays have their pointers stored and must access main memory (maybe l1 cache since there's only ~7100 values to be used)
// t0 -> tn are temp registers used for either subroutines or array accesses
// All rvals should have their value stored in a register called rout

use crate::lexer::Token;

use super::parser::Cond;
use super::parser::Expr;

#[derive(Debug)]
pub enum ASM {
    // mov src dst
    MOV(String, String),
    // set dst imm
    SETI(String, i64),
    SETS(String, String),
    SETB(String, bool),
    SETF(String, f64),

    // call f with args -> no need to pass args all values stay in the a predefined register
    CALL(String, Vec<String>),
    // add src dst -> dst = dst + src
    ADD(String, String),
    // sub src dst -> dst = dst - src
    SUB(String, String),
    // mul src dst -> dst = dst * src
    MUL(String, String),

    // div src dst -> dst = dst / src (float)
    DIV(String, String),

    // divi src dst -> dst = dst // src (int)
    DIVI(String, String),

    // and src dst -> dst = dst & src
    AND(String, String),

    // or src dst -> dst = dst & src
    OR(String, String),

    // xor src dst -> dst = dst xor src
    XOR(String, String),

    // seteq src dst -> if src = dst set rout to 1
    SETEQ(String, String),

    // setneq src dst -> if src != dst set rout to 1
    SETNEQ(String, String),

    // setg src dst -> if src > dst set rout to 1
    SETG(String, String),

    // setge src dst -> if src >= dst set rout to 1
    SETGE(String, String),

    // j label
    J(String),
    // jeq src dst label -> if src = dst goto label
    JEQ(String, String, String),
    // jneq src dst label -> if src != dst goto label
    JNEQ(String, String, String),
    // max src dst -> dst = max(src, dst)
    MAX(String, String),
    // label s
    LABEL(String),
    // end -> used to signal the end of a function
    // (i.e. return to previous address and continue execution)

    // Array definition
    DEF(String, usize, usize),

    // Primitives allowed in array declarations
    PRIMI(i64),
    PRIMB(bool),
    PRIMF(f64),
    PRIMS(String),

    // Array End Definitions (Initializations)
    ENDDEF,

    // cpu array access by pointer and using the dedicated registers
    // rarr{1, 2}
    ARRAYACCESS(String),

    END,
}

// Some operations should be done by the CPU itself
// when doing an array access, values are computed in asm
// and passed to the cpu via two dedicated registers
// array definitions and their sizes are precomputed
// their locations are similar to variables
// CPU OPs usually mean something to do with cache r/w

pub fn compile(ast: &Vec<Expr>) -> Vec<ASM> {
    let mut counter = 0;
    let mut v = vec![];
    for i in ast {
        v.extend(compile_expr(i.clone(), &mut counter));
    }
    return v;
}

pub fn compile_expr(expr: Expr, counter: &mut i32) -> Vec<ASM> {
    let mut v = vec![];
    match expr {
        Expr::Assign(s, e) => match *e {
            Expr::Array(_, s1, s2, toks) => {
                // CPU OP here to define the array and its size
                // CPU OPs here to set the values of the toks
                // in the cpu cache
                let mut q = vec![];
                q.push(ASM::DEF(format!("r({})", s), s1, s2));
                q.extend(compile_toks(toks));
                q.push(ASM::ENDDEF);
                v.extend(q);
            }
            _ => match *e.clone() {
                Expr::BinOp(x, n, w) => match (*n, *w) {
                    (Expr::VarID(z), Expr::Prim(Token::Int(y))) => {
                        if s == z && y == 1 {
                            let q = match x.as_str() {
                                "+" => {
                                    vec![ASM::ADD("r1".to_string(), format!("r({})", s))]
                                }
                                "-" => {
                                    vec![ASM::SUB("r1".to_string(), format!("r({})", s))]
                                }
                                "*" => {
                                    vec![ASM::MUL("r1".to_string(), format!("r({})", s))]
                                }
                                _ => {
                                    vec![]
                                }
                            };
                            v.extend(q);
                        }
                    }
                    _ => {
                        let mut q = compile_expr(*e.clone(), counter);
                        q.push(ASM::MOV("rout".to_string(), format!("r({})", s)));
                        v.extend(q);
                    }
                },

                _ => {
                    let mut q = compile_expr(*e, counter);
                    q.push(ASM::MOV("rout".to_string(), format!("r({})", s)));
                    v.extend(q);
                }
            },
        },
        Expr::If(c, exprs) => {
            let mut q = compile_cond(c, counter);
            q.push(ASM::JNEQ(
                "rout".to_string(),
                "r1".to_string(),
                "ifend".to_string(),
            ));
            q.extend(compile(&exprs));
            q.push(ASM::LABEL("ifend".to_string()));
            v.extend(q);
        }
        Expr::IfElse(c, e1, e2) => {
            let mut q = compile_cond(c, counter);
            q.push(ASM::JNEQ(
                "rout".to_string(),
                "r1".to_string(),
                "iffalse".to_string(),
            ));
            q.extend(compile(&e1));
            q.push(ASM::LABEL("iffalse".to_string()));
            q.extend(compile(&e2));
            v.extend(q);
        }
        Expr::Fun(x, _, e) => {
            let mut q = vec![ASM::LABEL(format!("f({x})"))];
            q.extend(compile(&e));
            q.push(ASM::END);
            v.extend(q);
        }
        Expr::Loop(e, b) => {
            let mut rng = rand::rng();
            let rand = rand::Rng::random::<u32>(&mut rng);
            let loop_str = format!("loop{}", rand);
            let mut q = vec![ASM::LABEL(loop_str.clone())];
            q.extend(compile(&e));
            q.extend(compile_cond(b.cond, counter));
            q.push(ASM::JNEQ("rout".to_string(), "r1".to_string(), loop_str));
            v.extend(q);
        }
        Expr::BinOp(x, _, _) => {
            match x.as_str() {
                "+" => {}
                "-" => {}
                "*" => {}
                "/" => {}
                "//" => {}
                "%" => {}

                _ => {}
            }
            // match some strings, rest are errors
        }
        Expr::FunCall(x, s) => {
            let q = vec![ASM::CALL(format!("f({x})"), s)];
            v.extend(q);
        }
        Expr::Prim(x) => {
            match x {
                Token::Int(z) => {
                    let q = vec![ASM::SETI("rout".to_string(), z)];
                    v.extend(q);
                }
                Token::Bool(b) => {
                    let q = vec![ASM::SETB("rout".to_string(), b)];
                    v.extend(q);
                }
                Token::Float(f) => {
                    let q = vec![ASM::SETF("rout".to_string(), f)];
                    v.extend(q);
                }
                Token::String(s) => {
                    let q = vec![ASM::SETS("rout".to_string(), s)];
                    v.extend(q);
                }
                _ => {}
            }
            // match some tokens, rest are errors
        }
        // should never get here arrays should be handled in assign
        Expr::Array(_, _, _, _) => {}

        Expr::ArrayAccess(s, e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "rarr1".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::MOV("rout".to_string(), "rarr2".to_string()));
            // CPU OP here with array name and rarr{1, 2}
            q.push(ASM::ARRAYACCESS(format!("r({})", s)));
            v.extend(q);
        }
        Expr::VarID(s) => {
            v.push(ASM::MOV(format!("r({})", s), "rout".to_string()));
        }
    }

    return v;
}

pub fn compile_cond(c: Cond, counter: &mut i32) -> Vec<ASM> {
    let mut v = vec![];
    match c {
        Cond::Eq(e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::SETEQ("rout".to_string(), "t0".to_string()));
            v.extend(q);
        }
        Cond::Neq(e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::SETNEQ("rout".to_string(), "t0".to_string()));
            v.extend(q);
        }
        Cond::Ge(e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::SETG("rout".to_string(), "t0".to_string()));
            v.extend(q);
        }
        Cond::Geq(e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::SETGE("rout".to_string(), "t0".to_string()));
            v.extend(q);
        }
        Cond::Leq(e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::SETG("rout".to_string(), "t0".to_string()));
            // Check if setg returns 0 and thus it must be leq
            q.push(ASM::SETEQ("rout".to_string(), "r0".to_string()));
            v.extend(q);
        }
        Cond::Le(e1, e2) => {
            let mut q = compile_expr(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_expr(*e2, counter));
            q.push(ASM::SETGE("rout".to_string(), "t0".to_string()));
            // Check if setge returns 0 and thus it must be le
            q.push(ASM::SETEQ("rout".to_string(), "r0".to_string()));
            v.extend(q);
        }
        Cond::Or(e1, e2) | Cond::LogicalOr(e1, e2) => {
            let mut q = compile_cond(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_cond(*e2, counter));
            q.push(ASM::OR("t0".to_string(), "rout".to_string()));
            v.extend(q);
        }
        Cond::And(e1, e2) | Cond::LogicalAnd(e1, e2) => {
            let mut q = compile_cond(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_cond(*e2, counter));
            q.push(ASM::AND("t0".to_string(), "rout".to_string()));
            v.extend(q);
        }
        Cond::Xor(e1, e2) => {
            let mut q = compile_cond(*e1, counter);
            q.push(ASM::MOV("rout".to_string(), "t0".to_string()));
            q.extend(compile_cond(*e2, counter));
            q.push(ASM::XOR("t0".to_string(), "rout".to_string()));
            v.extend(q);
        }
        Cond::Not(e) | Cond::LogicalNot(e) => {
            let mut q = compile_cond(*e, counter);
            // Check if setge returns 0 and thus it must be le
            q.push(ASM::SETEQ("rout".to_string(), "r0".to_string()));
            v.extend(q);
        }
    }

    return v;
}

pub fn compile_toks(toks: Vec<Token>) -> Vec<ASM> {
    let v = toks
        .into_iter()
        .map(|x| match x {
            Token::Int(z) => ASM::PRIMI(z),
            Token::Bool(b) => ASM::PRIMB(b),
            Token::Float(f) => ASM::PRIMF(f),
            Token::String(s) => ASM::PRIMS(s),
            _ => ASM::END,
        })
        .collect();

    return v;
}
