use super::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    // (ID) = (Expr)
    Assign(ID, Box<Expr>),
    // if (Cond) { Expr }
    // Maybe change to Vec<Expr>
    If(Cond, Box<Expr>),
    // if (Cond) { Expr } else { Expr }
    IfElse(Cond, Box<Expr>, Box<Expr>),
    // fun id(arg0, arg1, ...) { Expr }
    Fun(ID, Vec<ID>, Box<Expr>),
    // loop { Exprs +  Break + Exprs? }
    Loop(Vec<Box<Expr>>, Break),
    // Expr1 Binop Expr2
    BinOp(String, Box<Expr>, Box<Expr>),
    // id(arg0, arg1, ...)
    FunCall(ID, Vec<ID>),
    // int/string/bool
    Prim(Token),
    // variable that matches regex
    // [a-zA-Z][a-zA-Z0-9]*
    VarID(ID),
}

#[derive(Debug)]
// Variable IDs with names
pub struct ID {
    name: String,
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
                let mut end_pos = match temp_toks.iter().position(|x| match x {
                    Token::RCurly => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => {
                        return Err("Expected {} after if keyword");
                    }
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
                let end_pos = match temp_toks.iter().position(|x| match x {
                    Token::RCurly => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => {
                        return Err("Expected {} after if keyword");
                    }
                };

                let v = temp_toks.drain(..=end_pos);
                v.collect::<Vec<Token>>()
            }
            // Same as if but no special cases
            Token::Loop => {
                let end_pos = match temp_toks.iter().position(|x| match x {
                    Token::RCurly => true,
                    _ => false,
                }) {
                    Some(i) => i,
                    None => {
                        return Err("Expected {} after if keyword");
                    }
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
        // println!("Current Tokens {:?}", temp_toks);
        println!("{}) Tokens to parse {:?}", count, tokens_for_expr);

        // Create ast for a specific list of tokens
        let token_ast = create_ast(tokens_for_expr);
        println!("{:?}", token_ast);
        match token_ast {
            Ok(t) => exprs.push(t),
            Err(e) => return Err(e),
        }

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
        // function call not implemented yet
        // binop not implemented yet
        Token::Var(v) => {
            if tokens_for_expr.len() == 1 {
                return Ok(Expr::VarID(ID { name: v }));
            }

            let second = lookahead(&tokens_for_expr, 1);
            match second {
                Token::Assign => {
                    let a = create_ast(tokens_for_expr[2..].to_vec());
                    match a {
                        Ok(t) => Ok(Expr::Assign(ID { name: v }, Box::new(t))),
                        Err(e) => return Err(e),
                    }
                }
                _ => Err("Unhandled case function call"),
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
            println!("Cond for If Statement: {:?}", cond);

            let sl_start = match tokens_for_expr.iter().position(|x| match x {
                Token::Else => true,
                _ => false,
            }) {
                Some(i) => {
                    // Found else statement
                    // unimplemented
                    return Ok(Expr::IfElse(
                        cond,
                        Box::new(Expr::Prim(Token::Int(64))),
                        Box::new(Expr::Prim(Token::Int(64))),
                    ));
                }
                None => {
                    let cur_start = match tokens_for_expr.iter().position(|x| match x {
                        Token::LCurly => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected left curly after if condition"),
                    };
                    let cur_end = match tokens_for_expr.iter().position(|x| match x {
                        Token::RCurly => true,
                        _ => false,
                    }) {
                        Some(i) => i,
                        None => return Err("Expected right curly after if condition"),
                    };
                    let a = create_ast(tokens_for_expr[cur_start + 1..cur_end].to_vec());
                    return match a {
                        Ok(t) => Ok(Expr::If(cond, Box::new(t))),
                        Err(e) => Err(e),
                    };
                }
            };
        }

        // Functions and Loops unhandled
        // Token::Fun => {

        // },
        // Token::Loop => {

        // }

        // Binop not handled

        // Primitives handled
        Token::Int(x) => Ok(Expr::Prim(Token::Int(x))),
        Token::Bool(x) => Ok(Expr::Prim(Token::Bool(x))),
        Token::String(x) => Ok(Expr::Prim(Token::String(x))),

        // On unknown tokens
        _ => {
            println!("Unknown list of tokens");
            Ok(Expr::Assign(
                ID {
                    name: "test".to_string(),
                },
                Box::new(Expr::Prim(Token::Int(64))),
            ))
        }
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
