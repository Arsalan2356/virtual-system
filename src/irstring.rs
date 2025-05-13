use crate::lexer::Token;
use crate::parser::Break;
use crate::parser::Cond;
use crate::parser::Expr;

pub fn stringify(exprs: &Vec<Expr>) -> Result<Vec<String>, &'static str> {
    // Expr -> String
    let mut expr_strings = vec![];
    for e in exprs {
        expr_strings.push(convert_to_string(e.clone(), 0));
    }
    return Ok(expr_strings);
}

pub fn convert_to_string(expr: Expr, indent: usize) -> String {
    match expr {
        Expr::Assign(s, e) => {
            return match *e {
                Expr::Array(_, _, _, _) => {
                    format!(
                        "{}{} = {{\n{}}}\n",
                        " ".repeat(indent),
                        s,
                        convert_to_string(*e, indent + 4)
                    )
                }
                _ => format!(
                    "{}{} = {}\n",
                    " ".repeat(indent),
                    s,
                    convert_to_string(*e, 0)
                ),
            };
        }
        Expr::If(c, exprs) => {
            let new_indent = indent + 4;
            let mut expr_strings = vec![];
            for e in exprs {
                expr_strings.push(convert_to_string(e, new_indent));
            }
            let mut v = String::new();
            let cond_s = cond_to_string(c, indent);
            v.push_str(&cond_s);
            v.push_str(&format!(
                "{}if %cond, label %iffalse\n",
                " ".repeat(new_indent)
            ));
            v.push_str(&expr_strings.join(""));
            v.push_str(&format!("{}iffalse:\n", " ".repeat(new_indent)));
            return v;
        }
        Expr::IfElse(c, true_exprs, false_exprs) => {
            let new_indent = indent + 4;
            let mut true_strings: Vec<String> = vec![];
            for e in true_exprs {
                true_strings.push(convert_to_string(e, new_indent));
            }
            let mut false_strings: Vec<String> = vec![];
            for e in false_exprs {
                false_strings.push(convert_to_string(e, new_indent));
            }
            let mut v = String::new();
            let cond_s = cond_to_string(c, indent);
            v.push_str(&cond_s);
            v.push_str(&format!(
                "{}if %cond 1, label %iftrue, label %iffalse\n",
                " ".repeat(new_indent)
            ));
            v.push_str(&format!("{}iftrue:\n", " ".repeat(new_indent)));
            v.push_str(&true_strings.join(""));
            v.push_str(&format!("{}j %ifend\n", " ".repeat(new_indent)));
            v.push_str(&format!("{}iffalse:\n", " ".repeat(new_indent)));
            v.push_str(&false_strings.join(""));
            v.push_str(&format!("{}ifend:\n", " ".repeat(new_indent)));
            return v;
        }
        Expr::Fun(s, ids, exprs) => {
            let new_indent = indent + 4;
            let mut expr_strings = vec![];
            for e in exprs {
                expr_strings.push(convert_to_string(e, new_indent));
            }
            let mut v = String::new();
            v.push_str(&format!(
                "{}def {}({}) {{\n",
                " ".repeat(indent),
                s,
                ids.join(", ")
            ));
            v.push_str(&expr_strings.join(""));
            v.push_str(&format!("{}}}\n", " ".repeat(indent)));
            return v;
        }
        Expr::Loop(exprs, b) => {
            let new_indent = indent + 4;
            let mut expr_strings = vec![];
            for e in exprs {
                expr_strings.push(convert_to_string(e, new_indent));
            }
            let mut v = String::new();
            v.push_str(&format!("{}loop:\n", " ".repeat(indent)));
            v.push_str(&expr_strings.join(""));
            v.push_str(&break_to_string(b, new_indent));
            v.push_str(&format!(
                "{}if %break 0, label %loop\n",
                " ".repeat(new_indent),
            ));
            return v;
        }

        Expr::FunCall(s, ids) => {
            return format!("{}call {}({})\n", " ".repeat(indent), s, ids.join(", "));
        }

        Expr::Prim(t) => match t {
            Token::Int(x) => return format!("{}{}", " ".repeat(indent), x),
            Token::Float(f) => return format!("{}{}", " ".repeat(indent), f),
            Token::Bool(b) => return format!("{}{}", " ".repeat(indent), b),
            _ => return "unknown".to_owned(),
        },
        Expr::VarID(x) => x,
        Expr::BinOp(op, x, y) => {
            return format!(
                "{} {} {}",
                convert_to_string(*x, 0),
                op,
                convert_to_string(*y, 0)
            );
        }
        Expr::Array(_, n, m, el) => {
            let mut v = "".to_string();
            for i in 0..n {
                v += &" ".repeat(indent);
                for j in 0..m {
                    let s = match &el[i * m + j] {
                        Token::Float(x) => format!("{:.2}", x),
                        Token::Int(x) => format!("{}", x),
                        Token::String(x) => format!("{}", x),
                        Token::Bool(x) => format!("{}", x),
                        _ => "".to_owned(),
                    };
                    v.push_str(&s);
                    v += " ";
                }
                v += "\n";
            }
            return v;
        }
        Expr::ArrayAccess(x, e1, e2) => {
            let mut v = "".to_string();
            v.push_str(&format!("{}", " ".repeat(indent)));
            v.push_str(x.as_str());
            v.push('[');
            v.push_str(convert_to_string(*e1, 0).as_str());
            v.push(']');
            v.push('[');
            v.push_str(convert_to_string(*e2, 0).as_str());
            v.push(']');
            v += "\n";

            return v;
        }
    }
}

pub fn cond_to_string(cond: Cond, indent: usize) -> String {
    return match cond {
        Cond::Eq(x, y) => {
            format!(
                "{}%cond = eq {} {}\n",
                " ".repeat(indent),
                convert_to_string(*x, 0),
                convert_to_string(*y, 0)
            )
        }
        Cond::Neq(x, y) => {
            format!(
                "{}%cond = neq {} {}\n",
                " ".repeat(indent),
                convert_to_string(*x, 0),
                convert_to_string(*y, 0)
            )
        }
        Cond::Geq(x, y) => {
            format!(
                "{}%cond = geq {} {}\n",
                " ".repeat(indent),
                convert_to_string(*x, 0),
                convert_to_string(*y, 0)
            )
        }
        Cond::Leq(x, y) => {
            format!(
                "{}%cond = leq {} {}\n",
                " ".repeat(indent),
                convert_to_string(*x, 0),
                convert_to_string(*y, 0)
            )
        }
        Cond::Ge(x, y) => {
            format!(
                "{}%cond = ge {} {}\n",
                " ".repeat(indent),
                convert_to_string(*x, 0),
                convert_to_string(*y, 0)
            )
        }
        Cond::Le(x, y) => {
            format!(
                "{}%cond = le {} {}\n",
                " ".repeat(indent),
                convert_to_string(*x, 0),
                convert_to_string(*y, 0)
            )
        }
        Cond::Or(x, y) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            let w = format!("{}%cond2 = {}\n", " ".repeat(indent), cond_to_string(*y, 0));
            format!("{}{}{}%cond = or %cond1 %cond2\n", z, w, " ".repeat(indent))
        }
        Cond::And(x, y) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            let w = format!("{}%cond2 = {}\n", " ".repeat(indent), cond_to_string(*y, 0));
            format!(
                "{}{}{}%cond = and %cond1 %cond2\n",
                z,
                w,
                " ".repeat(indent)
            )
        }
        Cond::Not(x) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            format!("{}{}%cond = not %cond1\n", z, " ".repeat(indent))
        }
        Cond::Xor(x, y) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            let w = format!("{}%cond2 = {}\n", " ".repeat(indent), cond_to_string(*y, 0));
            format!(
                "{}{}{}%cond = xor %cond1 %cond2\n",
                z,
                w,
                " ".repeat(indent)
            )
        }
        Cond::LogicalOr(x, y) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            let w = format!("{}%cond2 = {}\n", " ".repeat(indent), cond_to_string(*y, 0));
            format!(
                "{}{}{}%cond = bitwiseor %cond1 %cond2\n",
                z,
                w,
                " ".repeat(indent)
            )
        }
        Cond::LogicalAnd(x, y) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            let w = format!("{}%cond2 = {}\n", " ".repeat(indent), cond_to_string(*y, 0));
            format!(
                "{}{}{}%cond = bitwiseand %cond1 %cond2\n",
                z,
                w,
                " ".repeat(indent)
            )
        }
        Cond::LogicalNot(x) => {
            let z = format!("{}%cond1 = {}\n", " ".repeat(indent), cond_to_string(*x, 0));
            format!("{}{}%cond = bitwisenot %cond1\n", z, " ".repeat(indent))
        }
    };
}

pub fn break_to_string(b: Break, indent: usize) -> String {
    let z = cond_to_string(b.cond, indent);
    return z.replace("%cond", "%break");
}
