use std::collections::HashMap;
use std::ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign};

use super::compiler::ASM;

#[derive(Debug)]
pub struct CPUState {
    pub inst_executed: i64,
    pub labels: Vec<(String, usize)>,
    pub vars: Vec<(String, usize)>,
    pub arrays: Vec<(String, usize, usize, usize)>,
    pub functions: Vec<(String, usize)>,
    pub vars_vals: HashMap<String, Prim>,
    pub arr_vals: HashMap<i64, Vec<Prim>>,
}

#[derive(Debug, Clone)]
pub enum Prim {
    Int(i64),
    Str(String),
    Float(f64),
    Bool(bool),
}

impl Mul for Prim {
    type Output = Self;

    fn mul(self, rhs: Prim) -> Self::Output {
        match (self, rhs) {
            (Prim::Int(x), Prim::Int(y)) => Prim::Int(x * y),
            (Prim::Float(x), Prim::Float(y)) => Prim::Float(x * y),
            (Prim::Bool(x), Prim::Bool(y)) => Prim::Bool((x as i64 * y as i64) != 0),
            (Prim::Str(x), Prim::Str(y)) => {
                let mut v = x.clone();
                v.push_str(y.as_str());
                Prim::Str(v)
            }
            (Prim::Int(x), Prim::Float(f)) => Prim::Float(x as f64 * f),
            (Prim::Float(f), Prim::Int(x)) => Prim::Float(x as f64 * f),
            _ => Prim::Int(0),
        }
    }
}

impl Add for Prim {
    type Output = Self;

    fn add(self, rhs: Prim) -> Self::Output {
        match (self, rhs) {
            (Prim::Int(x), Prim::Int(y)) => Prim::Int(x + y),
            (Prim::Float(x), Prim::Float(y)) => Prim::Float(x + y),
            (Prim::Bool(x), Prim::Bool(y)) => Prim::Bool((x as i64 + y as i64) != 0),
            (Prim::Str(x), Prim::Str(y)) => {
                let mut v = x.clone();
                v.push_str(y.as_str());
                Prim::Str(v)
            }
            (Prim::Int(x), Prim::Float(f)) => Prim::Float(x as f64 + f),
            (Prim::Float(f), Prim::Int(x)) => Prim::Float(x as f64 + f),
            _ => Prim::Int(0),
        }
    }
}

impl Sub for Prim {
    type Output = Self;

    fn sub(self, rhs: Prim) -> Self::Output {
        match (self, rhs) {
            (Prim::Int(x), Prim::Int(y)) => Prim::Int(x - y),
            (Prim::Float(x), Prim::Float(y)) => Prim::Float(x - y),
            (Prim::Bool(x), Prim::Bool(y)) => Prim::Bool((x as i64 - y as i64) != 0),
            (Prim::Str(x), Prim::Str(y)) => {
                let mut v = x.clone();
                v.push_str(y.as_str());
                Prim::Str(v)
            }
            (Prim::Int(x), Prim::Float(f)) => Prim::Float(x as f64 - f),
            (Prim::Float(f), Prim::Int(x)) => Prim::Float(f - x as f64),
            _ => Prim::Int(0),
        }
    }
}

impl AddAssign for Prim {
    fn add_assign(&mut self, other: Self) {
        match (&self, other) {
            (Prim::Int(x), Prim::Int(y)) => *self = Prim::Int(*x + y),
            (Prim::Float(x), Prim::Float(y)) => *self = Prim::Float(*x + y),
            (Prim::Bool(x), Prim::Bool(y)) => {
                let v = *x as i64 + y as i64;
                *self = Prim::Bool(v != 0);
            }
            (Prim::Str(x), Prim::Str(y)) => {
                let mut v = x.clone();
                v.push_str(y.as_str());
                *self = Prim::Str(v);
            }
            (Prim::Int(x), Prim::Float(f)) => *self = Prim::Float(*x as f64 + f),
            (Prim::Float(f), Prim::Int(x)) => *self = Prim::Float(x as f64 + *f),
            _ => (),
        }
    }
}

impl SubAssign for Prim {
    fn sub_assign(&mut self, other: Self) {
        match (&self, other) {
            (Prim::Int(x), Prim::Int(y)) => *self = Prim::Int(*x - y),
            (Prim::Float(x), Prim::Float(y)) => *self = Prim::Float(*x - y),
            (Prim::Bool(x), Prim::Bool(y)) => {
                let v = *x as i64 - y as i64;
                *self = Prim::Bool(v != 0);
            }
            (Prim::Str(x), Prim::Str(y)) => {
                let mut v = x.clone();
                v.push_str(y.as_str());
                *self = Prim::Str(v);
            }
            (Prim::Int(x), Prim::Float(f)) => *self = Prim::Float(*x as f64 - f),
            (Prim::Float(f), Prim::Int(x)) => *self = Prim::Float(*f - x as f64),
            _ => (),
        }
    }
}

impl MulAssign for Prim {
    fn mul_assign(&mut self, other: Self) {
        match (&self, other) {
            (Prim::Int(x), Prim::Int(y)) => *self = Prim::Int(*x * y),
            (Prim::Float(x), Prim::Float(y)) => *self = Prim::Float(*x * y),
            (Prim::Bool(x), Prim::Bool(y)) => {
                let v = *x as i64 * y as i64;
                *self = Prim::Bool(v != 0);
            }
            (Prim::Str(x), Prim::Str(y)) => {
                let mut v = x.clone();
                v.push_str(y.as_str());
                *self = Prim::Str(v);
            }
            (Prim::Int(x), Prim::Float(f)) => *self = Prim::Float(*x as f64 * f),
            (Prim::Float(f), Prim::Int(x)) => *self = Prim::Float(x as f64 * *f),
            _ => (),
        }
    }
}

pub fn process(asm: Vec<ASM>) -> CPUState {
    let mut inst_count = 0;
    let mut labels = vec![];
    let mut vars: Vec<(String, usize)> = vec![];
    let mut arrays: Vec<(String, usize, usize, usize)> = vec![];
    let mut functions: Vec<(String, usize)> = vec![];

    for x in 0..asm.len() {
        match &asm[x] {
            ASM::MOV(_, z) => {
                if z.contains("r(") {
                    let p = z[2..z.len() - 1].to_string();
                    let found = vars.iter().any(|x| x.0 == p);
                    if !found {
                        vars.push((p, x));
                    }
                }
            }
            ASM::DEF(z, size1, size2) => {
                if z.contains("r(") {
                    let p = z[2..z.len() - 1].to_string();
                    let found = arrays.iter().any(|x| x.0 == p);
                    if !found {
                        arrays.push((p, x, *size1, *size2));
                    }
                }
            }
            ASM::LABEL(v) => labels.push((v.clone(), x)),
            ASM::CALL(z, _) => {
                let p = z[2..z.len() - 1].to_string();
                let found = functions.iter().any(|x| x.0 == p);
                if z.contains("f(") && !found {
                    functions.push((p, x));
                }
            }
            _ => {}
        }
    }

    let mut index = 0;
    let mut vars_vals: HashMap<String, Prim> = HashMap::new();
    let mut arr_vals: HashMap<i64, Vec<Prim>> = HashMap::new();
    for i in &vars {
        vars_vals.insert(i.0.clone(), Prim::Int(0));
    }
    for i in &arrays {
        let mut rng = rand::rng();
        let rand = rand::Rng::random::<u32>(&mut rng);
        vars_vals.insert(i.0.clone(), Prim::Int(rand as i64));
        arr_vals.insert(rand as i64, vec![]);
    }

    let mut temps = vec![Prim::Int(0); 10];
    let mut rout = 0;
    let mut rarr1 = 0;
    let mut rarr2 = 0;

    while index < asm.len() {
        let curr_inst = &asm[index];
        match curr_inst {
            ASM::MOV(s1, s2) => {
                // Add check for differentiating between different register formats
                // r({}), tN, rout, rarr1, rarr2
                let p1 = if s1.len() > 2 {
                    s1[2..s1.len() - 1].to_string()
                } else {
                    s1.to_string()
                };
                let p2 = if s2.len() > 2 {
                    s2[2..s2.len() - 1].to_string()
                } else {
                    s2.to_string()
                };

                if s1.as_str() == "rout" {
                    if p2.chars().nth(0).unwrap() == 't' {
                        let x = p2
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x] = Prim::Int(rout);
                    } else {
                        vars_vals.insert(p2, Prim::Int(rout));
                    }
                } else if s2.as_str() == "rout" {
                    if p1.chars().nth(0).unwrap() == 't' {
                        let x = p1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        rout = match temps[x].clone() {
                            Prim::Int(x) => x,
                            Prim::Float(x) => x as i64,
                            Prim::Bool(b) => b as i64,
                            Prim::Str(s) => s.parse::<i64>().unwrap(),
                        }
                    } else {
                        rout = match vars_vals[&p1].clone() {
                            Prim::Int(x) => x,
                            Prim::Float(x) => x as i64,
                            Prim::Bool(b) => b as i64,
                            Prim::Str(s) => s.parse::<i64>().unwrap(),
                        }
                    }
                } else {
                    vars_vals.insert(p2, vars_vals[&p1].clone());
                }
            }
            ASM::SETI(s, i) => {
                let found = vars.iter().any(|x| x.0 == *s);
                if s.as_str() == "rout" {
                    rout = *i;
                } else {
                    if !found {
                        let x = s
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x] = Prim::Int(*i);
                    } else {
                        vars_vals.insert(s.clone(), Prim::Int(*i));
                    }
                }
            }
            ASM::SETB(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                if s1.as_str() == "rout" {
                    rout = *s2 as i64;
                } else {
                    if !found {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x] = Prim::Bool(*s2);
                    } else {
                        vars_vals.insert(s1.clone(), Prim::Bool(*s2));
                    }
                }
            }
            ASM::SETS(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                if s1.as_str() == "rout" {
                    rout = s1.parse::<i64>().unwrap();
                } else {
                    if !found {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x] = Prim::Str(s2.clone());
                    } else {
                        vars_vals.insert(s1.clone(), Prim::Str(s2.clone()));
                    }
                }
            }
            ASM::SETF(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);

                if s1.as_str() == "rout" {
                    rout = *s2 as i64;
                } else {
                    if !found {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x] = Prim::Float(*s2);
                    } else {
                        vars_vals.insert(s1.clone(), Prim::Float(*s2));
                    }
                }
            }
            ASM::CALL(f, args) => {
                let fname = f[2..f.len() - 1].to_string();
                runfunc(
                    &fname,
                    args,
                    &vars_vals,
                    &mut arr_vals,
                    &arrays,
                    &mut inst_count,
                );
            }
            ASM::ADD(s1, s2) => {
                let p = if s1.len() > 2 {
                    s1[2..s1.len() - 1].to_string()
                } else {
                    s1.to_string()
                };
                let found = vars.iter().any(|x| x.0 == p);
                let val1 = if !found {
                    if s1.as_str() == "r1" {
                        Prim::Int(1)
                    } else {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x].clone()
                    }
                } else {
                    vars_vals[&p].clone()
                };
                let p = if s2.len() > 2 {
                    s2[2..s2.len() - 1].to_string()
                } else {
                    s2.to_string()
                };
                let found = vars.iter().any(|x| x.0 == p);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(x + y),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Float(x + y),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }
            ASM::SUB(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    if s1.as_str() == "r1" {
                        Prim::Int(1)
                    } else {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x].clone()
                    }
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let p2 = if s2.len() > 2 {
                    s2[2..s2.len() - 1].to_string()
                } else {
                    s2.to_string()
                };
                let found = vars.iter().any(|x| x.0 == p2);
                let val2 = if !found {
                    if s2.as_str() == "r1" {
                        Prim::Int(1)
                    } else {
                        let x = s2
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x].clone()
                    }
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(y - x),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Float(y - x),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }
            ASM::MUL(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(x * y),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Float(x * y),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }
            ASM::DIV(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Float(x as f64 / y as f64),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Float(x / y),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }
            ASM::DIVI(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(x / y),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Int((x / y) as i64),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }
            ASM::AND(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(x & y),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Int(x as i64 & y as i64),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }
            ASM::OR(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(x | y),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Int(x as i64 | y as i64),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }

            ASM::XOR(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(x ^ y),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Int(x as i64 ^ y as i64),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }

            ASM::SETEQ(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    if s1.as_str() == "rout" {
                        Prim::Int(rout)
                    } else {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x].clone()
                    }
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    if s2.as_str() == "rout" {
                        Prim::Int(rout)
                    } else {
                        let x = s2
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x].clone()
                    }
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let test = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => x == y,
                    (Prim::Float(x), Prim::Float(y)) => x == y,
                    (Prim::Str(x), Prim::Str(y)) => x == y,
                    (Prim::Bool(x), Prim::Bool(y)) => x == y,
                    _ => false,
                };
                if test {
                    rout = 1;
                }
            }

            ASM::SETNEQ(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let test = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => x == y,
                    (Prim::Float(x), Prim::Float(y)) => x == y,
                    (Prim::Str(x), Prim::Str(y)) => x == y,
                    (Prim::Bool(x), Prim::Bool(y)) => x == y,
                    _ => false,
                };
                if !test {
                    rout = 1;
                }
            }

            ASM::SETG(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let test = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => x > y,
                    (Prim::Float(x), Prim::Float(y)) => x > y,
                    (Prim::Str(x), Prim::Str(y)) => x > y,
                    (Prim::Bool(x), Prim::Bool(y)) => x > y,
                    _ => false,
                };
                if test {
                    rout = 1;
                }
            }

            ASM::SETGE(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let test = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => x >= y,
                    (Prim::Float(x), Prim::Float(y)) => x >= y,
                    (Prim::Str(x), Prim::Str(y)) => x >= y,
                    (Prim::Bool(x), Prim::Bool(y)) => x >= y,
                    _ => false,
                };
                if test {
                    rout = 1;
                }
            }

            ASM::J(s) => {
                for (x, y) in &labels {
                    if *s == *x {
                        index = *y;
                        break;
                    }
                }
            }

            ASM::JEQ(s1, s2, s3) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let test = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => x == y,
                    (Prim::Float(x), Prim::Float(y)) => x == y,
                    (Prim::Str(x), Prim::Str(y)) => x == y,
                    (Prim::Bool(x), Prim::Bool(y)) => x == y,
                    _ => false,
                };

                if test {
                    for (x, y) in &labels {
                        if *s3 == *x {
                            index = *y;
                            break;
                        }
                    }
                }
            }

            ASM::JNEQ(s1, s2, s3) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    if s1.as_str() == "rout" {
                        Prim::Int(rout)
                    } else {
                        let x = s1
                            .chars()
                            .nth(1)
                            .unwrap()
                            .to_string()
                            .parse::<usize>()
                            .unwrap();
                        temps[x].clone()
                    }
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    if s2.as_str() == "rout" {
                        Prim::Int(rout)
                    } else {
                        if s2.as_str() == "r1" {
                            Prim::Int(1)
                        } else {
                            let x = s2
                                .chars()
                                .nth(1)
                                .unwrap()
                                .to_string()
                                .parse::<usize>()
                                .unwrap();
                            temps[x].clone()
                        }
                    }
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let test = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => x == y,
                    (Prim::Float(x), Prim::Float(y)) => x == y,
                    (Prim::Str(x), Prim::Str(y)) => x == y,
                    (Prim::Bool(x), Prim::Bool(y)) => x == y,
                    _ => false,
                };

                if !test {
                    for (x, y) in &labels {
                        if *s3 == *x {
                            index = *y;
                            break;
                        }
                    }
                }
            }

            ASM::MAX(s1, s2) => {
                let found = vars.iter().any(|x| x.0 == *s1);
                let val1 = if !found {
                    let x = s1
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s1[2..s1.len() - 1].to_string();
                    vars_vals[&p].clone()
                };
                let found = vars.iter().any(|x| x.0 == *s2);
                let val2 = if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x].clone()
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals[&p].clone()
                };

                let out = match (val1, val2) {
                    (Prim::Int(x), Prim::Int(y)) => Prim::Int(if x >= y { x } else { y }),
                    (Prim::Float(x), Prim::Float(y)) => Prim::Float(if x >= y { x } else { y }),
                    (Prim::Str(x), Prim::Str(y)) => Prim::Str(if x >= y { x } else { y }),
                    (Prim::Bool(x), Prim::Bool(y)) => Prim::Bool(if x >= y { x } else { y }),
                    _ => Prim::Int(0),
                };

                if !found {
                    let x = s2
                        .chars()
                        .nth(1)
                        .unwrap()
                        .to_string()
                        .parse::<usize>()
                        .unwrap();
                    temps[x] = out;
                } else {
                    let p = s2[2..s2.len() - 1].to_string();
                    vars_vals.insert(p.clone(), out);
                }
            }

            ASM::LABEL(_) => {}

            ASM::DEF(s, _, _) => {
                for (x, y) in &vars_vals {
                    let p = s[2..s.len() - 1].to_string();
                    if p == *x {
                        rout = match y {
                            &Prim::Int(z) => z,
                            _ => 0,
                        };
                        break;
                    }
                }
            }

            ASM::PRIMI(x) => {
                arr_vals.get_mut(&rout).unwrap().push(Prim::Int(*x));
            }
            ASM::PRIMB(x) => {
                arr_vals.get_mut(&rout).unwrap().push(Prim::Bool(*x));
            }
            ASM::PRIMF(x) => {
                arr_vals.get_mut(&rout).unwrap().push(Prim::Float(*x));
            }
            ASM::PRIMS(x) => {
                arr_vals.get_mut(&rout).unwrap().push(Prim::Str(x.clone()));
            }

            ASM::ENDDEF => {}

            ASM::ARRAYACCESS(s) => {
                let x = &vars_vals[s];
                match x {
                    Prim::Int(z) => {
                        let k = arr_vals.get_mut(&z).unwrap();
                        let mut arr_dim1 = 1;
                        for (x, _, s1, _) in &arrays {
                            if *x == *s {
                                arr_dim1 = *s1;
                                break;
                            }
                        }

                        temps[9] = k[rarr1 * arr_dim1 + rarr2].clone();
                    }
                    _ => {}
                }
            }

            ASM::END => {}
        }

        index += 1;
        inst_count += 1;
    }

    let finalstate = CPUState {
        inst_executed: inst_count,
        labels,
        vars,
        arrays,
        functions,
        vars_vals,
        arr_vals,
    };

    return finalstate;
}

pub fn runfunc(
    f: &String,
    args: &Vec<String>,
    vars_vals: &HashMap<String, Prim>,
    arr_vals: &mut HashMap<i64, Vec<Prim>>,
    arrays: &Vec<(String, usize, usize, usize)>,
    inst_count: &mut i64,
) {
    match f.as_str() {
        "matmulindex" => {
            let sample = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => x,
                _ => 0,
            };
            let sample_arr = arr_vals[&sample].clone();
            let index = match vars_vals[args[1].as_str()].clone() {
                Prim::Int(x) => x as usize,
                _ => 0,
            };
            let weights = match vars_vals[args[2].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };
            let out = match vars_vals[args[3].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };
            let mut sample_dim1 = 0;
            let mut sample_dim2 = 0;
            for (x, _, s1, s2) in arrays {
                if x == args[0].as_str() {
                    sample_dim1 = *s1;
                    sample_dim2 = *s2;
                    break;
                }
            }
            let mut weights_dim1 = 0;
            let mut weights_dim2 = 0;
            for (x, _, s1, s2) in arrays {
                if x == args[2].as_str() {
                    weights_dim1 = *s1;
                    weights_dim2 = *s2;
                    break;
                }
            }
            let start_index = sample_dim2 * index;
            for j in 0..out.len() {
                let mut s = sample_arr[start_index].clone() * weights[0].clone();
                for q in 1..sample_dim2 {
                    let s_ind = j * weights_dim1;
                    let v = sample_arr[start_index + q].clone() * weights[s_ind + q].clone();
                    s += v;
                    *inst_count += 2;
                }
                out[j] = s;
                *inst_count += 1;
            }
        }
        "relu" => {
            let weights = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };
            for x in weights {
                *x = match x {
                    Prim::Int(i) => Prim::Int(if *i > 0 {
                        *i
                    } else {
                        *inst_count += 1;
                        0
                    }),
                    Prim::Float(f) => Prim::Float(if *f > 0.0 {
                        *f
                    } else {
                        *inst_count += 1;
                        0.0
                    }),
                    Prim::Bool(b) => Prim::Bool(*b),
                    Prim::Str(s) => Prim::Str(s.clone()),
                };
            }
        }
        "arrayassign" => {
            let assign = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => x,
                _ => 0,
            };
            let assign_arr = arr_vals.get_mut(&assign).unwrap();

            let x = args[1].parse::<usize>().unwrap();
            let y = args[2].parse::<usize>().unwrap();

            let mut assign_dim1 = 0;
            let mut assign_dim2 = 0;
            for (x, _, s1, s2) in arrays {
                if x == args[0].as_str() {
                    assign_dim1 = *s1;
                    assign_dim2 = *s2;
                    break;
                }
            }
            let z = args[3].as_str().parse::<i64>().unwrap();

            assign_arr[x * assign_dim2 + y] = Prim::Int(z);
            *inst_count += 1;
        }
        "matmul" => {
            let mut a_dim1 = 0;
            let mut a_dim2 = 0;
            let mut b_dim1 = 0;
            let mut b_dim2 = 0;
            for (x, _, s1, s2) in arrays {
                if x == args[0].as_str() {
                    a_dim1 = *s1;
                    a_dim2 = *s2;
                }
                if x == args[1].as_str() {
                    b_dim1 = *s1;
                    b_dim2 = *s2;
                }
            }

            let a = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };
            let b = match vars_vals[args[1].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };

            let out = match vars_vals[args[2].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };

            for x in 0..a_dim1 {
                for y in 0..b_dim2 {
                    let mut s = a[x * a_dim2].clone() + b[y].clone();
                    for k in 1..a_dim2 {
                        s += a[x * a_dim2 + k].clone() * b[k * b_dim2 + y].clone();
                        *inst_count += 2;
                    }
                    out[x * b_dim2 + y] = s;
                    *inst_count += 1;
                }
            }
        }
        "sigmoid" => {
            // Only called on z3
            let arr = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };
            arr[0] = match arr[0].clone() {
                Prim::Int(x) => {
                    let f = (-x) as f64;
                    Prim::Float(1.0 / (1.0 + f.exp()))
                }
                Prim::Float(x) => Prim::Float(1.0 / 1.0 + (-x).exp()),
                _ => Prim::Float(0.5),
            };
            *inst_count += 2;
        }
        "matsubindex" => {
            let arr = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };
            let y = match vars_vals[args[1].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };

            let out = match vars_vals[args[3].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };

            let j = match vars_vals[args[2].as_str()].clone() {
                Prim::Int(x) => x as usize,
                _ => 0,
            };

            out[0] = arr[0].clone() - y[j].clone();
            *inst_count += 1;
        }
        "outer" => {
            let a = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };
            let b = match vars_vals[args[1].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };

            let mut out_dim1 = 0;
            let mut out_dim2 = 0;
            for (x, _, s1, s2) in arrays {
                if *x == args[2] {
                    out_dim1 = *s1;
                    out_dim2 = *s2;
                }
            }

            let out = match vars_vals[args[2].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };

            for x in 0..out_dim1 {
                for y in 0..out_dim2 {
                    out[x * out_dim2 + y] = a[x].clone() + b[y].clone();
                    *inst_count += 1;
                }
            }
        }
        "reludrv" => {
            let m = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };
            for i in 0..m.len() {
                m[i] = match &m[i] {
                    Prim::Int(x) => {
                        if *x > 0 {
                            Prim::Int(1)
                        } else {
                            Prim::Int(0)
                        }
                    }
                    Prim::Float(x) => {
                        if *x > 0.0 {
                            Prim::Int(1)
                        } else {
                            Prim::Int(0)
                        }
                    }
                    Prim::Bool(x) => {
                        if *x {
                            Prim::Int(1)
                        } else {
                            Prim::Int(0)
                        }
                    }
                    _ => Prim::Int(1),
                };
                *inst_count += 1;
            }
        }
        "elemmult" => {
            let b = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };
            let a = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };
            for x in 0..b.len() {
                a[x] *= b[x].clone();
                *inst_count += 1;
            }
        }
        "matmulconst" => {
            let b = vars_vals[args[1].as_str()].clone();
            let a = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };

            for x in 0..a.len() {
                a[x] *= b.clone();
                *inst_count += 1;
            }
        }
        "matsubinplace" => {
            let b = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals[&x].clone(),
                _ => vec![],
            };
            let a = match vars_vals[args[0].as_str()].clone() {
                Prim::Int(x) => arr_vals.get_mut(&x).unwrap(),
                _ => &mut vec![],
            };

            for x in 0..a.len() {
                a[x] -= b[x].clone();
                *inst_count += 1;
            }
        }

        _ => {
            // user-defined function
        }
    }
}
