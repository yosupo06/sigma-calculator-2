use num::{integer::lcm, BigRational, One, Zero};

use crate::{
    function::{Function, FunctionData},
    polynomial::{linear_polynomial::LinearPolynomial, polynomial::Polynomial},
    variable::Variable,
};
/*
    fn to_source_lines(&self) -> Vec<String> {
        let shift = |v: Vec<String>| -> Vec<String> {
            v.into_iter().map(|x| "  ".to_string() + &x).collect()
        };
        match self.data() {
            FunctionData::Bool { f } => vec![format!("({})", f)],
            FunctionData::PolynomialAsInt { p } => vec![format!("({})", p)],
            FunctionData::Add { l, r, .. } => {
                let mut ls = l.to_source_lines();
                let mut rs = r.to_source_lines();

                if ls.len() == 1 && rs.len() == 1 {
                    vec![format!("({}) + ({})", ls[0], rs[0])]
                } else {
                    ls.push("+".to_string());
                    ls.append(&mut rs);
                    ls
                }
            }
            FunctionData::Mul { l, r, .. } => {
                vec![format!(
                    "({} * {})",
                    l.to_source_s_line(),
                    r.to_source_s_line()
                )]
            }
            FunctionData::LoopSum { i, l, r, f, .. } => {
                let mut v = vec![format!(
                    "$({}_{} = {} .. {})",
                    i.name(),
                    i.id,
                    l.to_source_s_line(),
                    r.to_source_s_line(),
                )];
                v.append(&mut shift(f.to_source_lines()));
                v
            }
            FunctionData::If { cond, f, .. } => {
                let mut v = vec![format!("[{}]", cond.to_source_s_line())];
                v.append(&mut shift(f.to_source_lines()));
                v
            }
            FunctionData::IntIsDivisor { l, r } => {
                vec![format!("({} % {} == 0)", Polynomial::from(l.clone()), r,)]
            }
            FunctionData::IntIsNotNeg { p } => {
                vec![format!("({} >= 0)", Polynomial::from(p.clone()))]
            }
            FunctionData::Neg { v } => vec![format!("-({})", v.to_source_s_line())],
            FunctionData::Not { v } => vec![format!("!({})", v.to_source_s_line())],
            FunctionData::Declare { name, args, body } => {
                let mut v = vec![format!("{}({:?})=", name, args)];
                v.append(&mut shift(body.to_source_lines()));
                v
            }
        }
    }

    pub fn to_source(&self) -> String {
        self.to_source_lines().join("\n")
    }
    pub fn to_source_s_line(&self) -> String {
        self.to_source_lines().join(" ")
    }
*/

fn print_polynomial<'e>(p: &Polynomial<Variable<'e>>) -> String {
    if p.is_zero() {
        return "0".to_string();
    }
    let g = p
        .iter()
        .map(|(_, c)| c.denom().clone())
        .reduce(|x, y| lcm(x, y))
        .unwrap();

    let result = p
        .iter()
        .map(|(m, c)| {
            let coef = c.numer() * g.clone() / c.denom();
            if m.is_one() {
                format!("Int({})", coef)
            } else {
                let s = m
                    .iter()
                    .map(|(v, r)| (0..*r).map(|_| v.name()).collect::<Vec<String>>().join("*"))
                    .collect::<Vec<String>>()
                    .join("*");
                format!("{}*{}", coef, s)
            }
        })
        .collect::<Vec<String>>()
        .join("+");

    if g.is_one() {
        result
    } else {
        format!("({}) / {}", result, g)
    }
}

fn print_linear_polynomial<'e>(p: &LinearPolynomial<Variable<'e>, BigRational>) -> String {
    if p.is_zero() {
        return "0".to_string();
    }
    let g = p
        .iter()
        .map(|(_, c)| c.denom().clone())
        .reduce(|x, y| lcm(x, y))
        .unwrap();

    let result = p
        .iter()
        .map(|(v, c)| {
            let coef = c.numer() * g.clone() / c.denom();
            if let Some(v) = v {
                format!("{}*{}", coef, v.name())
            } else {
                format!("Int({})", coef)
            }
        })
        .collect::<Vec<String>>()
        .join("+");

    if g.is_one() {
        result
    } else {
        format!("({}) / {}", result, g)
    }
}

fn to_cpp_source_lines<'e>(f: &Function<'e>) -> Vec<String> {
    let shift =
        |v: Vec<String>| -> Vec<String> { v.into_iter().map(|x| "  ".to_string() + &x).collect() };
    match f.data() {
        FunctionData::PolynomialAsInt { p } => vec![format!("sum += ({});", print_polynomial(p))],
        FunctionData::Add { l, r, .. } => {
            let mut ls = to_cpp_source_lines(l);
            let mut rs = to_cpp_source_lines(r);
            ls.append(&mut rs);
            ls
        }
        FunctionData::If { cond, f, .. } => {
            let mut v = vec![format!("if ({}) {{", to_cpp_source(cond))];
            v.append(&mut shift(to_cpp_source_lines(f)));
            v.push("}".to_string());
            v
        }
        FunctionData::IntIsDivisor { l, r } => {
            vec![format!("({}) % {} == 0", print_linear_polynomial(l), r)]
        }
        FunctionData::IntIsNotNeg { p } => {
            vec![format!("{} >= 0", print_linear_polynomial(p))]
        }
        _ => todo!(),
    }
}
fn to_cpp_source<'e>(f: &Function<'e>) -> String {
    to_cpp_source_lines(f).join("\n")
}

pub fn cpp_print<'e>(f: &Function<'e>) -> String {
    if let FunctionData::Declare { name, args, body } = f.data() {
        let mut s: String = "".to_string();
        s += &format!(
            "Int {}({}) {{\n",
            name,
            args.iter()
                .map(|v| format!("Int {}", v.name()))
                .collect::<Vec<String>>()
                .join(", ")
        );
        s += &"Int sum = Int(0);\n";
        s += &to_cpp_source(body);
        s += &"\nreturn sum;\n";
        s += &"\n}\n";
        s
    } else {
        unreachable!()
    }
}
