use regex::Regex;

#[derive(Debug, Clone)]
pub enum Token {
    // Delimiters
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,
    Assign,
    // Comparison Operators
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    // Boolean Operators
    Or,
    And,
    Not,
    Xor,
    LogicalOr,
    LogicalAnd,
    LogicalNot,
    // Binary Operators
    Add,
    Sub,
    Mult,
    Div,
    IntDiv,
    Mod,
    // Control Flow
    If,
    Else,
    Rec,
    Fun,
    Return,
    Loop,
    Break,
    // Primitives
    Int(i64),
    Bool(bool),
    String(String),
    // Variable
    Var(String),
}

pub fn tokenize(str: String) -> Result<Vec<Token>, String> {
    let re_lparen = Regex::new(r"^\((\s*)").unwrap();
    let re_rparen = Regex::new(r"^\)(\s*)").unwrap();
    let re_lcurly = Regex::new(r"^\{(\s*)").unwrap();
    let re_rcurly = Regex::new(r"^\}(\s*)").unwrap();
    let re_semi = Regex::new(r"^;(\s*)").unwrap();
    let re_assign = Regex::new(r"^=(\s*)").unwrap();
    let re_eq = Regex::new(r"^==(\s*)").unwrap();
    let re_neq = Regex::new(r"^!=(\s*)").unwrap();
    let re_greater = Regex::new(r"^>(\s*)").unwrap();
    let re_less = Regex::new(r"^<(\s*)").unwrap();
    let re_geq = Regex::new(r"^>=(\s*)").unwrap();
    let re_leq = Regex::new(r"^<=(\s*)").unwrap();
    let re_or = Regex::new(r"^\|\|(\s*)").unwrap();
    let re_and = Regex::new(r"^&&(\s*)").unwrap();
    let re_not = Regex::new(r"^!(\s*)").unwrap();
    let re_xor = Regex::new(r"^\^(\s*)").unwrap();
    let re_logicalor = Regex::new(r"^\|(\s*)").unwrap();
    let re_logicaland = Regex::new(r"^&(\s*)").unwrap();
    let re_logicalnot = Regex::new(r"^~(\s*)").unwrap();
    let re_add = Regex::new(r"^\+(\s*)").unwrap();
    let re_sub = Regex::new(r"^-(\s*)").unwrap();
    let re_mult = Regex::new(r"^\*(\s*)").unwrap();
    let re_div = Regex::new(r"^/(\s*)").unwrap();
    let re_intdiv = Regex::new(r"^//(\s*)").unwrap();
    let re_mod = Regex::new(r"^%(\s*)").unwrap();
    let re_if = Regex::new(r"^if(\s*)").unwrap();
    let re_else = Regex::new(r"^else(\s*)").unwrap();
    let re_rec = Regex::new(r"^rec(\s*)").unwrap();
    let re_fun = Regex::new(r"^fun(\s*)").unwrap();
    let re_return = Regex::new(r"^return(\s*)").unwrap();
    let re_loop = Regex::new(r"^loop(\s*)").unwrap();
    let re_break = Regex::new(r"^break(\s*)").unwrap();
    // Primitives
    let re_int = Regex::new(r"^-?[0-9]+(\s*)").unwrap();
    let re_bool = Regex::new(r"^(true|false)(\s*)").unwrap();
    let re_string = Regex::new("^\"([^\"]*)\"(\\s*)").unwrap();
    let re_var = Regex::new(r"^([a-zA-Z][a-zA-Z0-9]*)(\s*)").unwrap();

    let mut tokens: Vec<Token> = vec![];

    let mut s = str.as_str();

    while s != "" {
        if re_if.is_match(s) {
            let m = re_if.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::If);
        } else if re_else.is_match(s) {
            let m = re_else.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Else);
        } else if re_rec.is_match(s) {
            let m = re_rec.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Rec);
        } else if re_fun.is_match(s) {
            let m = re_fun.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Fun);
        } else if re_return.is_match(s) {
            let m = re_return.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Return);
        } else if re_assign.is_match(s) {
            let m = re_assign.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Assign);
        } else if re_loop.is_match(s) {
            let m = re_loop.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Loop);
        } else if re_break.is_match(s) {
            let m = re_break.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Break);
        } else if re_int.is_match(s) {
            let m = re_int.find(s).unwrap();
            s = &s[m.end()..];
            let num = m.as_str().trim_end().parse().unwrap();
            tokens.push(Token::Int(num));
        } else if re_bool.is_match(s) {
            let m = re_bool.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Bool(if m.as_str().contains("true") {
                true
            } else {
                false
            }));
        } else if re_string.is_match(s) {
            let m = re_string.captures(s).unwrap();
            s = &s[m.get(0).unwrap().end()..];
            tokens.push(Token::String(m.get(1).unwrap().as_str().to_string()));
        } else if re_var.is_match(s) {
            let m = re_var.captures(s).unwrap();
            s = &s[m.get(0).unwrap().end()..];
            tokens.push(Token::Var(m.get(1).unwrap().as_str().to_string()));
        } else if re_mod.is_match(s) {
            let m = re_mod.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Mod);
        } else if re_intdiv.is_match(s) {
            let m = re_intdiv.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::IntDiv);
        } else if re_div.is_match(s) {
            let m = re_div.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Div);
        } else if re_mult.is_match(s) {
            let m = re_mult.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Mult);
        } else if re_sub.is_match(s) {
            let m = re_sub.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Sub);
        } else if re_add.is_match(s) {
            let m = re_add.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Add);
        } else if re_eq.is_match(s) {
            let m = re_eq.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Equal);
        } else if re_neq.is_match(s) {
            let m = re_neq.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::NotEqual);
        } else if re_geq.is_match(s) {
            let m = re_geq.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::GreaterEqual);
        } else if re_leq.is_match(s) {
            let m = re_leq.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::LessEqual);
        } else if re_greater.is_match(s) {
            let m = re_greater.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Greater);
        } else if re_less.is_match(s) {
            let m = re_less.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Less);
        } else if re_or.is_match(s) {
            let m = re_or.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Or);
        } else if re_and.is_match(s) {
            let m = re_and.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::And);
        } else if re_not.is_match(s) {
            let m = re_not.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Not);
        } else if re_xor.is_match(s) {
            let m = re_xor.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Xor);
        } else if re_logicalor.is_match(s) {
            let m = re_logicalor.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::LogicalOr);
        } else if re_logicaland.is_match(s) {
            let m = re_logicaland.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::LogicalAnd);
        } else if re_logicalnot.is_match(s) {
            let m = re_logicalnot.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::LogicalNot);
        } else if re_lparen.is_match(s) {
            let m = re_lparen.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::LParen);
        } else if re_rparen.is_match(s) {
            let m = re_rparen.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::RParen);
        } else if re_lcurly.is_match(s) {
            let m = re_lcurly.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::LCurly);
        } else if re_rcurly.is_match(s) {
            let m = re_rcurly.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::RCurly);
        } else if re_semi.is_match(s) {
            let m = re_semi.find(s).unwrap();
            s = &s[m.end()..];
            tokens.push(Token::Semicolon);
        } else {
            return Err("Unable to tokenize string".to_string());
        }
    }

    return Ok(tokens);
}
