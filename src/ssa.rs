use crate::lexer::Token;
use crate::parser::Break;
use crate::parser::Cond;
use crate::parser::Expr;

pub fn ssa(exprs: Vec<Expr>) -> Result<Vec<String>, &'static str> {
    // Expr -> String
    let mut ssa_strings = vec![];
    for e in exprs {
        ssa_strings.push(convert_to_string(e));
    }
    return Ok(ssa_strings);
}

pub fn convert_to_string(expr: Expr) -> String {
    match expr {
        Expr::Assign(s, e) => {
            let v = convert_to_string(*e);
            return format!("{} = {}\n", s, v);
        }
        Expr::If(c, exprs) => {
            let mut ssa_strings = vec![];
            for e in exprs {
                ssa_strings.push(convert_to_string(e));
            }
            let mut v = String::new();
            let cond_s = cond_to_string(c);
            v.push_str(&cond_s);
            v.push_str(&format!("if %cond, label %iffalse\n"));
            v.push_str(&ssa_strings.join(""));
            v.push_str("iffalse:\n");
            return v;
        }
        Expr::IfElse(c, true_exprs, false_exprs) => {
            let mut true_strings: Vec<String> = vec![];
            for e in true_exprs {
                true_strings.push(convert_to_string(e));
            }
            let mut false_strings: Vec<String> = vec![];
            for e in false_exprs {
                false_strings.push(convert_to_string(e));
            }
            let mut v = String::new();
            let cond_s = cond_to_string(c);
            v.push_str(&cond_s);
            v.push_str(&format!("if %cond 1, label %iftrue, label %iffalse\n"));
            v.push_str("iftrue:\n");
            v.push_str(&true_strings.join(""));
            v.push_str("j %ifend\n");
            v.push_str("iffalse:\n");
            v.push_str(&false_strings.join(""));
            v.push_str("ifend:\n");
            return v;
        }
        Expr::Fun(s, ids, exprs) => {
            let mut ssa_strings = vec![];
            for e in exprs {
                ssa_strings.push(convert_to_string(e));
            }
            let mut v = String::new();
            v.push_str(&format!("def {}({}) {{\n", s, ids.join(", ")));
            v.push_str(&ssa_strings.join(""));
            v.push_str("}\n");
            return v;
        }
        Expr::Loop(exprs, b) => {
            let mut ssa_strings = vec![];
            for e in exprs {
                ssa_strings.push(convert_to_string(e));
            }
            let mut v = String::new();
            v.push_str("loop:\n");
            v.push_str(&ssa_strings.join(""));
            v.push_str(&break_to_string(b));
            v.push_str("if %break 0, label %loop\n");
            return v;
        }

        Expr::FunCall(s, ids) => return format!("call {}({})\n", s, ids.join(", ")),

        Expr::Prim(t) => match t {
            Token::Int(x) => return format!("{}", x),
            Token::Float(f) => return format!("{}", f),
            Token::Bool(b) => return format!("{}", b),
            _ => return "unknown".to_owned(),
        },
        _ => return "".to_owned(),
    }
}

pub fn cond_to_string(cond: Cond) -> String {
    return "testcond\n".to_owned();
}

pub fn break_to_string(b: Break) -> String {
    return "testbreak\n".to_owned();
}
