use std::{char, collections::HashMap, str::FromStr};

use combine::{
    attempt, between, chainl1, many, many1, optional,
    parser::char::{alpha_num, digit, letter, spaces, string},
    sep_by, Parser, Stream,
};
use num::{BigInt, BigRational, One};

use crate::{
    constant::Type,
    function::Function,
    polynomial::{linear_polynomial::LinearPolynomial, polynomial::Polynomial},
    variable::{Variable, VariableManager},
};

enum Expr {
    Int {
        v: BigInt,
    },
    Variable {
        name: String,
    },
    Add {
        l: Box<Self>,
        r: Box<Self>,
    },
    Sub {
        l: Box<Self>,
        r: Box<Self>,
    },
    Mul {
        l: Box<Self>,
        r: Box<Self>,
    },
    Sum {
        v: String,
        l: Box<Self>,
        r: Box<Self>,
        f: Box<Self>,
    },
    Function {
        name: String,
        args: Vec<String>,
        f: Box<Self>,
    },
    If {
        cond: Box<Self>,
        f: Box<Self>,
    },
    Neg {
        v: Box<Self>,
    },
    LessEq {
        l: Box<Self>,
        r: Box<Self>,
    },
    IsDivisor {
        l: Box<Self>,
        r: Box<Self>,
    },
}
impl Expr {
    fn to_int<'e>(
        &self,
        _gen: &'e VariableManager,
        _vars: &HashMap<String, Variable<'e>>,
    ) -> Option<BigInt> {
        match self {
            Self::Int { v } => Some(v.clone()),
            _ => None,
        }
    }
    fn to_linear_polynomial<'e>(
        &self,
        gen: &'e VariableManager,
        vars: &HashMap<String, Variable<'e>>,
    ) -> Option<LinearPolynomial<Variable<'e>>> {
        match self {
            Self::Int { v } => Some(LinearPolynomial::from(BigRational::from(v.clone()))),
            Self::Variable { name } => {
                if let Some(v) = vars.get(name) {
                    Some(LinearPolynomial::from([(
                        Some(v.clone()),
                        BigRational::one(),
                    )]))
                } else {
                    None
                }
            }
            Self::Add { l, r } => {
                if let (Some(l), Some(r)) = (
                    l.to_linear_polynomial(gen, vars),
                    r.to_linear_polynomial(gen, vars),
                ) {
                    Some(l + r)
                } else {
                    None
                }
            }
            Self::Sub { l, r } => {
                if let (Some(l), Some(r)) = (
                    l.to_linear_polynomial(gen, vars),
                    r.to_linear_polynomial(gen, vars),
                ) {
                    Some(l - r)
                } else {
                    None
                }
            }
            Self::Mul { l, r } => {
                if let (Some(l), Some(r)) = (
                    l.to_linear_polynomial(gen, vars),
                    r.to_linear_polynomial(gen, vars),
                ) {
                    if let Some(l) = l.to_constant() {
                        Some(r * l)
                    } else if let Some(r) = r.to_constant() {
                        Some(l * r)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Self::Neg { v } => {
                if let Some(p) = v.to_linear_polynomial(gen, vars) {
                    Some(-p)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn to_functions<'e>(
        &self,
        gen: &'e VariableManager,
        vars: &HashMap<String, Variable<'e>>,
    ) -> Option<Function<'e>> {
        match self {
            Self::Int { v } => Some(Function::new_polynomial_as_int(Polynomial::from(
                BigRational::from(v.clone()),
            ))),
            Self::Variable { name } => {
                if let Some(v) = vars.get(name) {
                    Some(Function::new_polynomial_as_int(Polynomial::from([(
                        v.clone().into(),
                        BigRational::one(),
                    )])))
                } else {
                    None
                }
            }
            Self::Add { l, r } => {
                if let (Some(l), Some(r)) = (l.to_functions(gen, vars), r.to_functions(gen, vars)) {
                    l + r
                } else {
                    None
                }
            }
            Self::Sub { l, r } => {
                if let (Some(l), Some(r)) = (l.to_functions(gen, vars), r.to_functions(gen, vars)) {
                    l - r
                } else {
                    None
                }
            }
            Self::Mul { l, r } => {
                if let (Some(l), Some(r)) = (l.to_functions(gen, vars), r.to_functions(gen, vars)) {
                    l * r
                } else {
                    None
                }
            }
            Self::Neg { v } => {
                if let Some(v) = v.to_functions(gen, vars) {
                    Some(Function::new_neg(v))
                } else {
                    None
                }
            }
            Self::LessEq { l, r } => {
                if let (Some(l), Some(r)) = (
                    l.to_linear_polynomial(gen, vars),
                    r.to_linear_polynomial(gen, vars),
                ) {
                    Some(Function::new_int_is_not_neg(r - l))
                } else {
                    None
                }
            }
            Self::IsDivisor { l, r } => {
                if let (Some(l), Some(r)) = (l.to_int(gen, vars), r.to_linear_polynomial(gen, vars))
                {
                    Some(Function::new_int_is_divisor(r, l))
                } else {
                    None
                }
            }

            Self::Sum { v, l, r, f } => {
                if let (Some(l), Some(r)) = (l.to_functions(gen, vars), r.to_functions(gen, vars)) {
                    if l.return_type() != &Type::Integer || r.return_type() != &Type::Integer {
                        return None;
                    }
                    let mut vars = vars.clone();
                    let var = gen.new_var(v.clone());
                    vars.insert(v.clone(), var.clone());
                    if let Some(f) = f.to_functions(gen, &vars) {
                        Some(Function::new_loop_sum(var, l, r, f))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Self::If { cond, f } => {
                if let (Some(cond), Some(f)) =
                    (cond.to_functions(gen, vars), f.to_functions(gen, vars))
                {
                    Some(Function::new_if(cond, f))
                } else {
                    None
                }
            }
            Self::Function { name, args, f } => {
                let mut vars = vars.clone();
                let args = args
                    .iter()
                    .map(|v| {
                        let var = gen.new_var(v.clone());
                        vars.insert(v.clone(), var.clone());
                        var
                    })
                    .collect::<Vec<_>>();
                if let Some(f) = f.to_functions(gen, &vars) {
                    Some(Function::new_declare(name.clone(), args, f.clone()))
                } else {
                    None
                }
            }
        }
    }
}

pub fn parse<'e>(source: &str, gen: &'e VariableManager) -> Option<Function<'e>> {
    let result = function()
        .parse(source)
        .map(|x| x.0.to_functions(gen, &HashMap::new()));
    match result {
        Ok(v) => v,
        _ => None,
    }
}

fn op<I>(s: &'static str) -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    attempt(string(s)).skip(spaces()).map(|x| x.to_string())
}

fn variable_token<I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char>,
{
    letter()
        .and(many(alpha_num()))
        .skip(spaces())
        .map(|t: (char, String)| t.0.to_string() + &t.1)
}

fn function<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    (
        variable_token(),
        op("("),
        sep_by(variable_token(), op(",")),
        op(")"),
        op("="),
        expr(),
    )
        .map(|t| Expr::Function {
            name: t.0,
            args: t.2,
            f: Box::new(t.5),
        })
}

fn expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    expr_()
}

parser! {
    #[inline]
    fn expr_[Input]()(Input) -> Expr
        where [ Input: Stream<Token = char>]
    {
        add_sub_expr()
    }
}

fn add_sub_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    chainl1(
        mul_expr(),
        op("+").or(op("-")).map(|op| match op.as_str() {
            "+" => |l, r| Expr::Add {
                l: Box::new(l),
                r: Box::new(r),
            },
            "-" => |l, r| Expr::Sub {
                l: Box::new(l),
                r: Box::new(r),
            },
            _ => unreachable!(),
        }),
    )
}

fn mul_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    chainl1(
        sum_expr(),
        op("*").map(|_| {
            |l, r| Expr::Mul {
                l: Box::new(l),
                r: Box::new(r),
            }
        }),
    )
}

fn sum_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    let sum_expr = (
        op("$"),
        op("("),
        variable_token(),
        op("="),
        expr(),
        op(".."),
        expr(),
        op(")"),
        expr(),
    )
        .map(|t| Expr::Sum {
            v: t.2,
            l: Box::new(t.4),
            r: Box::new(t.6),
            f: Box::new(t.8),
        });
    sum_expr.or(if_expr())
}

fn if_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    let if_expr = (op("["), condition(), op("]"), expr()).map(|t| Expr::If {
        cond: Box::new(t.1),
        f: Box::new(t.3),
    });
    if_expr.or(parentheses_expr())
}

fn condition<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    condition_()
}
parser! {
    #[inline]
    fn condition_[Input]()(Input) -> Expr
        where [ Input: Stream<Token = char>]
    {
        let ops = choice!(
            op(">="),
            op("<="),
            op(">"),
            op("<"),
            op("|")
        );

        (
            add_sub_expr(),
            optional(ops.and(add_sub_expr()))
        ).map(|t|
            if let Some((op, r)) = t.1 {
                let l = t.0;
                match op.as_str() {
                    ">=" => Expr::LessEq { l: Box::new(r), r: Box::new(l) },
                    "<=" => Expr::LessEq { l: Box::new(l), r: Box::new(r) },
                    "|" => Expr::IsDivisor { l: Box::new(l), r: Box::new(r) },
                    _ => todo!()
                }
            } else {
                t.0
            })
    }
}

fn parentheses_expr<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    between(op("("), op(")"), expr()).or(term())
}

fn term<I>() -> impl Parser<I, Output = Expr>
where
    I: Stream<Token = char>,
{
    term_()
}

parser! {
    #[inline]
    fn term_[Input]()(Input) -> Expr
        where [ Input: Stream<Token = char>]
    {
        let neg_term = op("-").with(term()).map(|t| Expr::Neg { v: Box::new(t) });
        let int_term =     attempt(many1(digit()))
        .skip(spaces())
        .map(|x: String| Expr::Int {
            v: BigInt::from_str(&x).unwrap(),
        });
        let variable_term = variable_token().map(|x| Expr::Variable { name: x });
        choice!(
            neg_term,
            int_term,
            variable_term
        )
    }
}
