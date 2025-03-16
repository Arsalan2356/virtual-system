use lexer::tokenize;
pub mod lexer;

fn main() {
    let s = "
        (){};=!=;><;>=;<=;||&&!^|&~+-*/;//;%;
        if else def rec fun lambda loop;
        0 1 1 2 3 5 -1 -3 -5;
        true false;
        \"test string\";
        a12;
        ";

    let tokens = match tokenize(s.to_string().replace("\n", "").trim_ascii().to_string()) {
        Ok(t) => t,
        Err(e) => {
            println!("{:?}", e);
            vec![]
        }
    };

    println!("{:?}", tokens);

    println!("Hello, world!");
}
