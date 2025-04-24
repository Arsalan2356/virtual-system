use super::parser::Break;
use super::parser::Cond;
use super::parser::Expr;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub enum SSA {
    // Normal expressions remain unchanged
    S(Expr),
    If(Cond, Vec<SSA>),
    IfElse(Cond, Vec<SSA>, Vec<SSA>),
    // Phi Expressions are added after for control flow expressions
    Phi(String, String),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variable {
    basename: String,
    subscript: u32,
}

impl Variable {
    pub fn to_string(&self) -> String {
        return self.basename.clone() + "_sub_" + &self.subscript.to_string();
    }
}

pub fn to_ssa(exprs: &mut Vec<Expr>, vars: &mut HashSet<Variable>) -> Vec<SSA> {
    let mut ssa_exprs = vec![];
    // Store base-name for variables and their counter
    // When testing for variables, look if removing the string _sub_(count + 1) from a variable's name results in it matching basename
    // This does break if you create two variables x and x_sub_1 that aren't related in any way except name
    // The SSA writer assumes that the second is a continuation of the first in SSA form
    for i in 0..exprs.len() {
        let expr = exprs[i].clone();
        match expr {
            Expr::Assign(s, e) => {
                let mut split_str: Vec<&str> = s.split("_sub_").into_iter().collect();
                if split_str.len() == 1 {
                    split_str.push("0");
                }
                let mut var = Variable {
                    basename: split_str[0].to_string(),
                    subscript: split_str[1].parse::<u32>().unwrap_or(0),
                };
                if vars.contains(&var) {
                    let old = &var.to_string();
                    var.subscript += 1;
                    let var_name = var.to_string();
                    vars.insert(var.clone());
                    for j in i + 1..exprs.len() {
                        exprs[j] = replace_var(&exprs[j], old, &var.to_string());
                    }
                    ssa_exprs.push(SSA::S(Expr::Assign(var_name, e)));
                } else {
                    vars.insert(var.clone());
                    let var_name = var.to_string();
                    for j in i + 1..exprs.len() {
                        exprs[j] = replace_var(&exprs[j], &s, &var_name);
                    }
                    ssa_exprs.push(SSA::S(Expr::Assign(var_name, e)));
                }
            }
            Expr::If(cond, e) => {
                let new_exprs = handle_if(cond, e, vars);
                for i in new_exprs {
                    ssa_exprs.push(i);
                }
            }
            Expr::IfElse(cond, true_e, false_e) => {}
            Expr::Loop(e, break_val) => {}
            _ => ssa_exprs.push(SSA::S(expr)),
        }
    }

    return ssa_exprs;
}

pub fn handle_if(cond: Cond, e: Vec<Expr>, vars: &mut HashSet<Variable>) -> Vec<SSA> {
    let mut new_exprs = vec![];
    let mut expr_vars = vec![];

    for i in e.clone() {
        match i {
            Expr::Assign(s, _) => expr_vars.push(s.clone()),
            _ => {}
        }
    }

    let mut phi: HashMap<String, String> = HashMap::new();

    for s in expr_vars {
        let mut split_str: Vec<&str> = s.split("_sub_").into_iter().collect();
        if split_str.len() == 1 {
            split_str.push("0");
        }
        let var = Variable {
            basename: split_str[0].to_string(),
            subscript: split_str[1].parse::<u32>().unwrap_or(0),
        };
        if vars.contains(&var) {
            phi.insert(s.clone(), s.clone());
        }
    }

    let if_exprs = to_ssa(&mut e.clone(), vars);

    let phi_clone = phi.clone();
    let phi_keys = phi_clone.keys().collect::<Vec<&String>>();

    for p in phi_keys {
        let p_base = p.split("_sub_").collect::<Vec<&str>>()[0];
        let v = vars.clone().into_iter().max_by_key(|x| {
            if x.basename == p_base {
                100 + x.subscript
            } else {
                0
            }
        });
        match v {
            Some(k) => {
                phi.insert(p.clone(), k.to_string());
            }
            None => {}
        }
    }

    new_exprs.push(SSA::If(cond, if_exprs));
    for (l, r) in phi {
        new_exprs.push(SSA::Phi(l, r));
    }

    return new_exprs;
}

// Replace all instances of a variable with a new one
pub fn replace_var(expr: &Expr, old: &String, new: &String) -> Expr {
    return match expr {
        Expr::Assign(s, e) => {
            let new_e = replace_var(e, old, new);
            if s == old {
                Expr::Assign(new.clone(), Box::new(new_e))
            } else {
                Expr::Assign(s.clone(), Box::new(new_e))
            }
        }
        Expr::If(cond, exprs) => {
            let c2 = replace_cond_var(&cond, old, new);
            let e2 = exprs
                .into_iter()
                .map(|x| replace_var(&x, old, new))
                .collect();
            Expr::If(c2, e2)
        }
        Expr::IfElse(cond, true_exprs, false_exprs) => {
            let c2 = replace_cond_var(&cond, old, new);
            let e1 = true_exprs
                .into_iter()
                .map(|x| replace_var(&x, old, new))
                .collect();
            let e2 = false_exprs
                .into_iter()
                .map(|x| replace_var(&x, old, new))
                .collect();
            Expr::IfElse(c2, e1, e2)
        }
        Expr::BinOp(s, e1, e2) => {
            let e3 = replace_var(&e1, old, new);
            let e4 = replace_var(&e2, old, new);
            Expr::BinOp(s.clone(), Box::new(e3), Box::new(e4))
        }
        Expr::FunCall(s, vars) => {
            let new_v = vars
                .into_iter()
                .map(|x| if x == old { new.clone() } else { x.clone() })
                .collect();
            Expr::FunCall(s.clone(), new_v)
        }
        Expr::VarID(s) => {
            if *s == *old {
                Expr::VarID(new.clone())
            } else {
                expr.clone()
            }
        }
        Expr::Loop(e, break_val) => {
            let new_e = e.into_iter().map(|x| replace_var(x, old, new)).collect();
            let new_break = Break {
                cond: replace_cond_var(&break_val.cond, old, new),
            };

            Expr::Loop(new_e, new_break)
        }
        _ => expr.clone(),
    };
}

pub fn replace_cond_var(cond: &Cond, old: &String, new: &String) -> Cond {
    return match cond {
        Cond::Eq(e1, e2) => {
            let e3 = replace_var(e1, old, new);
            let e4 = replace_var(e2, old, new);
            Cond::Eq(Box::new(e3), Box::new(e4))
        }
        Cond::Neq(e1, e2) => {
            let e3 = replace_var(e1, old, new);
            let e4 = replace_var(e2, old, new);
            Cond::Neq(Box::new(e3), Box::new(e4))
        }
        Cond::Geq(e1, e2) => {
            let e3 = replace_var(e1, old, new);
            let e4 = replace_var(e2, old, new);
            Cond::Geq(Box::new(e3), Box::new(e4))
        }
        Cond::Leq(e1, e2) => {
            let e3 = replace_var(e1, old, new);
            let e4 = replace_var(e2, old, new);
            Cond::Leq(Box::new(e3), Box::new(e4))
        }
        Cond::Ge(e1, e2) => {
            let e3 = replace_var(e1, old, new);
            let e4 = replace_var(e2, old, new);
            Cond::Ge(Box::new(e3), Box::new(e4))
        }
        Cond::Le(e1, e2) => {
            let e3 = replace_var(e1, old, new);
            let e4 = replace_var(e2, old, new);
            Cond::Le(Box::new(e3), Box::new(e4))
        }
        Cond::Or(e1, e2) => {
            let e3 = replace_cond_var(e1, old, new);
            let e4 = replace_cond_var(e2, old, new);
            Cond::Or(Box::new(e3), Box::new(e4))
        }
        Cond::And(e1, e2) => {
            let e3 = replace_cond_var(e1, old, new);
            let e4 = replace_cond_var(e2, old, new);
            Cond::And(Box::new(e3), Box::new(e4))
        }
        Cond::Not(e1) => {
            let e2 = replace_cond_var(e1, old, new);
            Cond::Not(Box::new(e2))
        }
        Cond::Xor(e1, e2) => {
            let e3 = replace_cond_var(e1, old, new);
            let e4 = replace_cond_var(e2, old, new);
            Cond::Xor(Box::new(e3), Box::new(e4))
        }
        Cond::LogicalOr(e1, e2) => {
            let e3 = replace_cond_var(e1, old, new);
            let e4 = replace_cond_var(e2, old, new);
            Cond::LogicalOr(Box::new(e3), Box::new(e4))
        }
        Cond::LogicalAnd(e1, e2) => {
            let e3 = replace_cond_var(e1, old, new);
            let e4 = replace_cond_var(e2, old, new);
            Cond::LogicalAnd(Box::new(e3), Box::new(e4))
        }
        Cond::LogicalNot(e1) => {
            let e2 = replace_cond_var(e1, old, new);
            Cond::LogicalNot(Box::new(e2))
        }
    };
}
