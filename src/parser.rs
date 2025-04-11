use super::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    // (ID) = (Expr)
    Assign(String, Box<Expr>),
    // if (Cond) { Exprs }
    If(Cond, Vec<Expr>),
    // if (Cond) { Exprs } else { Exprs }
    IfElse(Cond, Vec<Expr>, Vec<Expr>),
    // fun id(arg0(,?) arg1(,?) ...) { Exprs }
    Fun(String, Vec<String>, Vec<Expr>),
    // loop { Exprs + Break + Exprs? }
    Loop(Vec<Expr>, Break),
    // Expr1 Binop Expr2
    BinOp(String, Box<Expr>, Box<Expr>),
    // id(arg0(,?) arg1(,?) ...)
    FunCall(String, Vec<String>),
    // int/string/bool
    Prim(Token),
    // Array of size n x m
    Array(String, usize, usize, Vec<Token>),
    // variable that matches regex
    // [a-zA-Z][a-zA-Z0-9]*
    VarID(String),
}

#[derive(Debug)]
// Break with a condition on when
pub struct Break {
    cond: Cond,
}

#[derive(Debug)]
// Cond = Cond BooleanExpr Cond | Eq | Neq | Geq | Leq | Ge | Le
pub enum Cond {
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Geq(Box<Expr>, Box<Expr>),
    Leq(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Or(Box<Cond>, Box<Cond>),
    And(Box<Cond>, Box<Cond>),
    Not(Box<Cond>),
    Xor(Box<Cond>, Box<Cond>),
    LogicalOr(Box<Cond>, Box<Cond>),
    LogicalAnd(Box<Cond>, Box<Cond>),
    LogicalNot(Box<Cond>),
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Expr>, &'static str> {
    let expr = create_expr(tokens);
    match expr {
        Ok(t) => {
            // Small properties of result implementing FromIterator
            let ast: Result<Vec<Expr>, &str> =
                t.iter().map(|x| return create_ast(x.clone())).collect();
            return ast;
        }
        Err(e) => return Err(e),
    }
}

fn find_curly(tokens: &[Token]) -> Result<usize, &'static str> {
    let mut q = 0;
    for i in 0..tokens.len() {
        match tokens[i] {
            Token::LCurly => q += 1,
            Token::RCurly => {
                if q == 0 {
                    return Ok(i);
                } else {
                    q -= 1
                }
            }
            _ => {}
        }
    }

    Err("No matching right curly found")
}

pub fn create_expr(tokens: Vec<Token>) -> Result<Vec<Vec<Token>>, &'static str> {
    let mut exprs = vec![];
    println!("{} tokens", tokens.len());

    let mut temp_toks = tokens.clone();
    // Keep counter to know how many exprs we have
    let mut count = 1;

    while !temp_toks.is_empty() {
        let t = temp_toks[0].clone();
        // Match on first token in list
        // Here we split based on semicolons for normal statements
        // For if/fun/loop we find right curly braces and split there
        // This doesn't do any tree-building, it only finds endpoints before we
        // create a list of exprs
        let tokens_for_expr = match t {
            Token::If => {
                // Look for a right curly brace and fail if not found
                // Write helper func using stack to find ending curly brace
                let start_curly = match temp_toks.iter().position(|x| match x {
                    Token::LCurly => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => return Err("Expected {} after if statement"),
                };

                let mut end_pos = match find_curly(&temp_toks[start_curly + 1..]) {
                    Ok(i) => i + start_curly + 1,
                    Err(e) => return Err(e),
                };

                // If we have extra tokens, look for an else
                // If so, we need an if-else not an if
                if end_pos + 1 < temp_toks.len() {
                    match temp_toks[end_pos + 1] {
                        Token::Else => {
                            // Skip first n tokens to find second right curly brace
                            let mut temp_arr_iter = temp_toks.iter().skip(end_pos + 1);
                            match temp_arr_iter.position(|x| match x {
                                Token::RCurly => true,
                                _ => false,
                            }) {
                                Some(i) => end_pos += 1 + i,
                                None => {
                                    return Err("Expected {} after else keyword");
                                }
                            }
                        }
                        _ => {}
                    }
                }

                let v = temp_toks.drain(..=end_pos);
                v.collect::<Vec<Token>>()
            }
            // Same as if but no special cases
            Token::Fun => {
                let start_curly = match temp_toks.iter().position(|x| match x {
                    Token::LCurly => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => return Err("Expected {} after fun statement"),
                };

                let end_pos = match find_curly(&temp_toks[start_curly + 1..]) {
                    Ok(i) => i + start_curly + 1,
                    Err(e) => return Err(e),
                };

                let v = temp_toks.drain(..=end_pos);
                v.collect::<Vec<Token>>()
            }
            // Same as if but no special cases
            Token::Loop => {
                let start_curly = match temp_toks.iter().position(|x| match x {
                    Token::LCurly => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => return Err("Expected {} after fun statement"),
                };

                let end_pos = match find_curly(&temp_toks[start_curly + 1..]) {
                    Ok(i) => i + start_curly + 1,
                    Err(e) => return Err(e),
                };

                let v = temp_toks.drain(..=end_pos);
                v.collect::<Vec<Token>>()
            }

            _ => {
                // Normal statements should have a semicolon after them
                // Break on that and send it to parse
                let end_tok = match temp_toks.iter().position(|x| match x {
                    Token::Semicolon => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => {
                        return Err("Expected semicolon after statement");
                    }
                };

                let v = temp_toks.drain(..=end_tok);
                v.collect::<Vec<Token>>()
            }
        };
        println!("{}) Tokens to parse {:?}", count, tokens_for_expr);

        // Create ast for a specific list of tokens
        exprs.push(tokens_for_expr);
        count += 1;
    }

    if exprs.len() == 0 {
        return Err("No Expressions in Tokens");
    }

    return Ok(exprs);
}

pub fn create_ast(tokens_for_expr: Vec<Token>) -> Result<Expr, &'static str> {
    fn lookahead(l: &Vec<Token>, n: usize) -> Token {
        return l[n].clone();
    }

    // Check first token to see what we have
    match lookahead(&tokens_for_expr, 0) {
        // If first token is var,
        // could be assign or a function call or a binop with a variable
        Token::Var(v) => {
            if tokens_for_expr.len() == 1 {
                return Ok(Expr::VarID(v));
            }

            let second = lookahead(&tokens_for_expr, 1);
            match second {
                Token::Assign => {
                    let a = create_ast(tokens_for_expr[2..].to_vec());
                    match a {
                        Ok(t) => Ok(Expr::Assign(v, Box::new(t))),
                        Err(e) => Err(e),
                    }
                }
                Token::Add | Token::Sub | Token::Mult | Token::Div | Token::IntDiv | Token::Mod => {
                    let b = create_ast(tokens_for_expr[2..].to_vec());
                    match b {
                        Ok(t) => match second {
                            Token::Add => Ok(Expr::BinOp(
                                "+".to_string(),
                                Box::new(Expr::VarID(v)),
                                Box::new(t),
                            )),
                            Token::Sub => Ok(Expr::BinOp(
                                "-".to_string(),
                                Box::new(Expr::VarID(v)),
                                Box::new(t),
                            )),
                            Token::Mult => Ok(Expr::BinOp(
                                "*".to_string(),
                                Box::new(Expr::VarID(v)),
                                Box::new(t),
                            )),
                            Token::Div => Ok(Expr::BinOp(
                                "/".to_string(),
                                Box::new(Expr::VarID(v)),
                                Box::new(t),
                            )),
                            Token::IntDiv => Ok(Expr::BinOp(
                                "//".to_string(),
                                Box::new(Expr::VarID(v)),
                                Box::new(t),
                            )),
                            Token::Mod => Ok(Expr::BinOp(
                                "%".to_string(),
                                Box::new(Expr::VarID(v)),
                                Box::new(t),
                            )),
                            _ => Err("Expected +,-,*,/,//,% after var"),
                        },
                        Err(e) => Err(e),
                    }
                }
                Token::LParen => {
                    // This is a function call instead of a variable call
                    // Should look like id(arg0(,?) arg1(,?) arg2(,?) ...)
                    let mut ids = vec![];
                    for i in 2..tokens_for_expr.len() {
                        match &tokens_for_expr[i] {
                            Token::Var(x) => ids.push(x.clone()),
                            _ => {}
                        }
                    }
                    Ok(Expr::FunCall(v, ids))
                }
                Token::Semicolon => Ok(Expr::VarID(v)),
                _ => Err("Unexpected Token after var"),
            }
        }
        // If statement,
        // Currently only handling without else
        Token::If => {
            let sl_start = match tokens_for_expr.iter().position(|x| match x {
                Token::LParen => true,
                _ => false,
            }) {
                Some(i) => i,
                None => return Err("Expected left paren after if keyword"),
            };
            let sl_end = match tokens_for_expr.iter().position(|x| match x {
                Token::RParen => true,
                _ => false,
            }) {
                Some(i) => i,
                None => return Err("Expected right paren after if keyword"),
            };
            let new_v = &tokens_for_expr[sl_start + 1..sl_end];

            let cond = match parse_cond(new_v) {
                Ok(t) => t,
                Err(e) => return Err(e),
            };

            match tokens_for_expr.iter().position(|x| match x {
                Token::Else => true,
                _ => false,
            }) {
                Some(_) => {
                    // Found else statement
                    let fstart_curly = match tokens_for_expr.iter().position(|x| match x {
                        Token::LCurly => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected left curly after if keyword"),
                    };
                    let fend_curly = match find_curly(&tokens_for_expr[fstart_curly + 1..]) {
                        Ok(i) => i + fstart_curly + 1,
                        Err(e) => return Err(e),
                    };

                    let else_toks = tokens_for_expr[fend_curly + 1..].to_vec();

                    let sstart_curly = match else_toks.iter().position(|x| match x {
                        Token::LCurly => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected left curly after if keyword"),
                    };
                    let send_curly = match find_curly(&else_toks[sstart_curly + 1..]) {
                        Ok(i) => i + sstart_curly + 1,
                        Err(e) => return Err(e),
                    };

                    let if_list =
                        match create_expr(tokens_for_expr[fstart_curly + 1..fend_curly].to_vec()) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        };

                    let else_list =
                        match create_expr(else_toks[sstart_curly + 1..send_curly].to_vec()) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        };

                    let if_exprs: Result<Vec<Expr>, &str> = if_list
                        .iter()
                        .map(|x| return create_ast(x.clone()))
                        .collect();

                    let else_exprs: Result<Vec<Expr>, &str> = else_list
                        .iter()
                        .map(|x| return create_ast(x.clone()))
                        .collect();

                    match if_exprs {
                        Ok(i) => match else_exprs {
                            Ok(ex) => Ok(Expr::IfElse(cond, i, ex)),
                            Err(e) => Err(e),
                        },
                        Err(e) => Err(e),
                    }
                }
                None => {
                    let cur_start = match tokens_for_expr.iter().position(|x| match x {
                        Token::LCurly => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected left curly after if condition"),
                    };
                    let cur_end = match find_curly(&tokens_for_expr[cur_start + 1..]) {
                        Ok(i) => i + cur_start + 1,
                        Err(e) => return Err(e),
                    };
                    // Set of tokens here could be multiple exprs
                    // First split into Vec of Vec<Token>
                    // Then create_ast for each one and assign that as value for the expr
                    let token_list = create_expr(tokens_for_expr[cur_start + 1..cur_end].to_vec());

                    match token_list {
                        Ok(v) => {
                            // Fun properties of result
                            // Since it impls FromIterator, we can switch the order
                            // So, Vec<Result> becomes Result<Vec> and we can directly know
                            // if there was an error in the {}
                            let expr_res_list: Result<Vec<Expr>, &str> =
                                v.iter().map(|x| return create_ast(x.clone())).collect();

                            match expr_res_list {
                                Ok(t) => Ok(Expr::If(cond, t)),
                                Err(e) => Err(e),
                            }
                        }
                        Err(e) => Err(e),
                    }
                }
            }
        }

        // Functions handled
        Token::Fun => {
            let id0 = match lookahead(&tokens_for_expr, 1) {
                Token::Var(x) => x,
                _ => {
                    return Err("Unnamed function");
                }
            };
            let end_index = match tokens_for_expr.iter().position(|x| match x {
                Token::RParen => true,
                _ => false,
            }) {
                Some(i) => i,
                None => return Err("Expected ) after function definition"),
            };

            let mut ids = vec![];
            for i in 2..end_index {
                match &tokens_for_expr[i] {
                    Token::Var(x) => ids.push(x.clone()),
                    _ => {}
                }
            }

            let start_curly = match tokens_for_expr.iter().position(|x| match x {
                Token::LCurly => true,
                _ => false,
            }) {
                Some(i) => i,
                None => return Err("Expected { after function arguments"),
            };
            let end_curly = match find_curly(&tokens_for_expr[start_curly + 1..]) {
                Ok(i) => i + start_curly + 1,
                Err(e) => return Err(e),
            };

            let exprs = match create_expr(tokens_for_expr[start_curly + 1..end_curly].to_vec()) {
                Ok(t) => t,
                Err(e) => return Err(e),
            };
            let expr_res_list: Result<Vec<Expr>, &str> =
                exprs.iter().map(|x| return create_ast(x.clone())).collect();

            match expr_res_list {
                Ok(t) => Ok(Expr::Fun(id0, ids, t)),
                Err(e) => Err(e),
            }
        }

        // Loop handled
        Token::Loop => {
            let start_curly = match tokens_for_expr.iter().position(|x| match x {
                Token::LCurly => true,
                _ => false,
            }) {
                Some(i) => i,
                None => return Err("Expected { after function arguments"),
            };
            let end_curly = match find_curly(&tokens_for_expr[start_curly + 1..]) {
                Ok(i) => i + start_curly + 1,
                Err(e) => return Err(e),
            };

            let mut exprs = match create_expr(tokens_for_expr[start_curly + 1..end_curly].to_vec())
            {
                Ok(t) => t,
                Err(e) => return Err(e),
            };
            let break_expr = match exprs.pop() {
                Some(i) => i,
                None => return Err("No statements in loop {}, put a break at least"),
            };
            let cond = match &break_expr[0] {
                Token::Break => {
                    let sl_start = match break_expr.iter().position(|x| match x {
                        Token::LParen => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected left paren after break keyword"),
                    };
                    let sl_end = match break_expr.iter().position(|x| match x {
                        Token::RParen => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected right paren after break keyword"),
                    };
                    parse_cond(&break_expr[sl_start + 1..sl_end])
                }
                _ => return Err("No break found in final line"),
            };
            match cond {
                Ok(c) => {
                    let expr_res_list: Result<Vec<Expr>, &str> =
                        exprs.iter().map(|x| return create_ast(x.clone())).collect();

                    match expr_res_list {
                        Ok(t) => Ok(Expr::Loop(t, Break { cond: c })),
                        Err(e) => Err(e),
                    }
                }
                Err(e) => Err(e),
            }
        }

        // Primitives handled
        Token::Int(_) | Token::Float(_) => {
            let t = lookahead(&tokens_for_expr, 0);
            if tokens_for_expr.len() > 1 {
                match lookahead(&tokens_for_expr, 1) {
                    Token::Add
                    | Token::Sub
                    | Token::Mult
                    | Token::Div
                    | Token::IntDiv
                    | Token::Mod => {
                        let v = lookahead(&tokens_for_expr, 1);
                        match v {
                            Token::Add => {
                                let e2 = create_ast(tokens_for_expr[2..].to_vec());
                                match e2 {
                                    Ok(e) => Ok(Expr::BinOp(
                                        "+".to_string(),
                                        Box::new(Expr::Prim(t)),
                                        Box::new(e),
                                    )),
                                    Err(e) => Err(e),
                                }
                            }
                            Token::Sub => {
                                let e2 = create_ast(tokens_for_expr[2..].to_vec());
                                match e2 {
                                    Ok(e) => Ok(Expr::BinOp(
                                        "-".to_string(),
                                        Box::new(Expr::Prim(t)),
                                        Box::new(e),
                                    )),
                                    Err(e) => Err(e),
                                }
                            }
                            Token::Mult => {
                                let e2 = create_ast(tokens_for_expr[2..].to_vec());
                                match e2 {
                                    Ok(e) => Ok(Expr::BinOp(
                                        "*".to_string(),
                                        Box::new(Expr::Prim(t)),
                                        Box::new(e),
                                    )),
                                    Err(e) => Err(e),
                                }
                            }
                            Token::Div => {
                                let e2 = create_ast(tokens_for_expr[2..].to_vec());
                                match e2 {
                                    Ok(e) => Ok(Expr::BinOp(
                                        "/".to_string(),
                                        Box::new(Expr::Prim(t)),
                                        Box::new(e),
                                    )),
                                    Err(e) => Err(e),
                                }
                            }
                            Token::IntDiv => {
                                let e2 = create_ast(tokens_for_expr[2..].to_vec());
                                match e2 {
                                    Ok(e) => Ok(Expr::BinOp(
                                        "//".to_string(),
                                        Box::new(Expr::Prim(t)),
                                        Box::new(e),
                                    )),
                                    Err(e) => Err(e),
                                }
                            }
                            Token::Mod => {
                                let e2 = create_ast(tokens_for_expr[2..].to_vec());
                                match e2 {
                                    Ok(e) => Ok(Expr::BinOp(
                                        "%".to_string(),
                                        Box::new(Expr::Prim(t)),
                                        Box::new(e),
                                    )),
                                    Err(e) => Err(e),
                                }
                            }
                            _ => Err("Expected +,-,*,/,//,% after var"),
                        }
                    }
                    _ => Ok(Expr::Prim(t)),
                }
            } else {
                Ok(Expr::Prim(t))
            }
        }
        Token::Bool(x) => Ok(Expr::Prim(Token::Bool(x))),
        Token::String(x) => Ok(Expr::Prim(Token::String(x))),

        // Vector
        Token::LCurly => {
            let vec_type = match lookahead(&tokens_for_expr, 1) {
                Token::Int(_) => "int",
                Token::Float(_) => "float",
                Token::Bool(_) => "bool",
                _ => "",
            }
            .to_string();

            if vec_type == "" {
                return Err("Unknown type in vector");
            }

            let mut vec_cols = 0;
            let mut toks = vec![];

            let mut c = 1;
            while c < tokens_for_expr.len() {
                match lookahead(&tokens_for_expr, c) {
                    Token::Comma => {
                        if vec_cols == 0 {
                            vec_cols = toks.len();
                        } else {
                            if toks.len() % vec_cols != 0 {
                                return Err("Mismatch in number of elements per row");
                            }
                        }
                    }
                    Token::Float(x) => {
                        if vec_type == "float" {
                            toks.push(Token::Float(x));
                        } else {
                            return Err("Mismatched type in vector, got float");
                        }
                    }
                    Token::Int(x) => {
                        if vec_type == "int" {
                            toks.push(Token::Int(x));
                        } else {
                            return Err("Mismatched type in vector, got int");
                        }
                    }
                    Token::Bool(x) => {
                        if vec_type == "bool" {
                            toks.push(Token::Bool(x));
                        } else {
                            return Err("Mismatched type in vector, got bool");
                        }
                    }
                    Token::RCurly | Token::Semicolon => {}
                    x => {
                        println!("Unexpected token in vector {:?}", x);
                        return Err("Unexpected token in vector");
                    }
                }
                c += 1;
            }

            let vec_rows = toks.len() / vec_cols;

            return Ok(Expr::Array(vec_type, vec_rows, vec_cols, toks));
        }

        // On unknown tokens
        _ => return Err("Unknown list of tokens"),
    }
}

pub fn parse_cond(v: &[Token]) -> Result<Cond, &'static str> {
    // Look for boolean operators to split into multiple conds
    let bool_search = v.iter().position(|x| match x {
        Token::Or
        | Token::And
        | Token::Not
        | Token::Xor
        | Token::LogicalOr
        | Token::LogicalAnd
        | Token::LogicalNot => true,
        _ => false,
    });
    match bool_search {
        Some(i) => {
            // Found boolean operator need to split
            // Handles all splitting and calling recursively
            match v[i] {
                Token::Or => {
                    let c1 = match parse_cond(&v[0..i]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    let c2 = match parse_cond(&v[i + 1..]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::Or(Box::new(c1), Box::new(c2)))
                }
                Token::And => {
                    let c1 = match parse_cond(&v[0..i]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    let c2 = match parse_cond(&v[i + 1..]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::And(Box::new(c1), Box::new(c2)))
                }
                Token::Not => {
                    let c1 = match parse_cond(&v[i..]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::Not(Box::new(c1)))
                }
                Token::Xor => {
                    let c1 = match parse_cond(&v[0..i]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    let c2 = match parse_cond(&v[i + 1..]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::Xor(Box::new(c1), Box::new(c2)))
                }
                Token::LogicalOr => {
                    let c1 = match parse_cond(&v[0..i]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    let c2 = match parse_cond(&v[i + 1..]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::LogicalOr(Box::new(c1), Box::new(c2)))
                }
                Token::LogicalAnd => {
                    let c1 = match parse_cond(&v[0..i]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    let c2 = match parse_cond(&v[i + 1..]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::LogicalAnd(Box::new(c1), Box::new(c2)))
                }
                Token::LogicalNot => {
                    let c1 = match parse_cond(&v[0..i]) {
                        Ok(t) => t,
                        Err(e) => return Err(e),
                    };
                    Ok(Cond::LogicalNot(Box::new(c1)))
                }
                // Should never get here
                _ => Err("Expected a boolean operator?"),
            }
        }

        // No boolean operator
        // Handle simple cases
        // Calls create_ast for the exprs inside the binops
        None => {
            let comp_pos = v.iter().position(|x| match x {
                Token::Equal
                | Token::NotEqual
                | Token::Greater
                | Token::Less
                | Token::GreaterEqual
                | Token::LessEqual => true,
                _ => false,
            });
            match comp_pos {
                Some(i) => {
                    let cond_op = &v[i];
                    match cond_op {
                        Token::Equal => {
                            let a1 = match create_ast(v[0..i].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            let a2 = match create_ast(v[i + 1..].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            return Ok(Cond::Eq(Box::new(a1), Box::new(a2)));
                        }
                        Token::NotEqual => {
                            let a1 = match create_ast(v[0..i].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            let a2 = match create_ast(v[i + 1..].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            return Ok(Cond::Neq(Box::new(a1), Box::new(a2)));
                        }

                        Token::GreaterEqual => {
                            let a1 = match create_ast(v[0..i].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            let a2 = match create_ast(v[i + 1..].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            return Ok(Cond::Geq(Box::new(a1), Box::new(a2)));
                        }

                        Token::LessEqual => {
                            let a1 = match create_ast(v[0..i].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            let a2 = match create_ast(v[i + 1..].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            return Ok(Cond::Leq(Box::new(a1), Box::new(a2)));
                        }

                        Token::Greater => {
                            let a1 = match create_ast(v[0..i].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            let a2 = match create_ast(v[i + 1..].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            return Ok(Cond::Ge(Box::new(a1), Box::new(a2)));
                        }

                        Token::Less => {
                            let a1 = match create_ast(v[0..i].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            let a2 = match create_ast(v[i + 1..].to_vec()) {
                                Ok(t) => t,
                                Err(e) => return Err(e),
                            };
                            return Ok(Cond::Le(Box::new(a1), Box::new(a2)));
                        }
                        _ => {
                            return Err("Should never get here");
                        }
                    }
                }
                None => return Err("No comparison operator in if"),
            }
        }
    }
}
