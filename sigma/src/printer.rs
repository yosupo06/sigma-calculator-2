use num::{integer::lcm, BigInt, BigRational, One, Zero};

use crate::{
    function::{Condition, Function, FunctionData, FunctionDeclare, IsDivisor, IsNotNeg},
    polynomials::{linear_polynomial::LinearPolynomial, polynomial::Polynomial},
    variable::Variable,
};

fn print_polynomial<'e>(p: &Polynomial<Variable<'e>, BigRational>) -> String {
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

fn print_condition<'e>(cond: &Condition<'e>) -> String {
    match cond {
        Condition::IsDivisor(IsDivisor { p, c }) => {
            format!("({}) % {} == 0", print_linear_polynomial(p), c)
        }
        Condition::IsNotNeg(IsNotNeg { p }) => {
            format!("{} >= 0", print_linear_polynomial(p))
        }
    }
}

fn print_linear_polynomial<'e>(p: &LinearPolynomial<Variable<'e>, BigInt>) -> String {
    if p.is_zero() {
        return "0".to_string();
    }
    let result = p
        .iter()
        .map(|(v, c)| {
            if let Some(v) = v {
                format!("{}*{}", c, v.name())
            } else {
                format!("Int({})", c)
            }
        })
        .collect::<Vec<String>>()
        .join("+");

    result
}

fn to_cpp_source_lines<'e>(f: &Function<'e>) -> Vec<String> {
    let shift =
        |v: Vec<String>| -> Vec<String> { v.into_iter().map(|x| "  ".to_string() + &x).collect() };
    match f.data() {
        FunctionData::Polynomial { p } => vec![format!("sum += ({});", print_polynomial(p))],
        FunctionData::Add { l, r, .. } => {
            let mut ls = to_cpp_source_lines(l);
            let mut rs = to_cpp_source_lines(r);
            ls.append(&mut rs);
            ls
        }
        FunctionData::If { cond, f, .. } => {
            let mut v = vec![format!("if ({}) {{", print_condition(cond))];
            v.append(&mut shift(to_cpp_source_lines(f)));
            v.push("}".to_string());
            v
        }
        _ => todo!(),
    }
}
fn to_cpp_source<'e>(f: &Function<'e>) -> String {
    to_cpp_source_lines(f).join("\n")
}

pub fn cpp_print<'e>(f: &FunctionDeclare<'e>) -> String {
    let mut s: String = "".to_string();
    s += &format!(
        "Int {}({}) {{\n",
        f.name,
        f.args
            .iter()
            .map(|v| format!("Int {}", v.name()))
            .collect::<Vec<String>>()
            .join(", ")
    );
    s += &"Int sum = Int(0);\n";
    s += &to_cpp_source(&f.body);
    s += &"\nreturn sum;\n";
    s += &"\n}\n";
    s
}
