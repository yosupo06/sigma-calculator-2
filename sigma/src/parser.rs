use std::{char, collections::HashMap, str::FromStr};

use combine::{
    attempt, between, chainl1, eof, many, many1, optional,
    parser::char::{alpha_num, digit, letter, spaces, string},
    sep_by, Parser, Stream,
};
use num::{BigInt, BigRational, One};

use crate::{
    function::{Condition, Function, FunctionDeclare, IsDivisor, IsNotNeg},
    polynomials::{linear_polynomial::LinearPolynomial, polynomial::Polynomial},
    variable::{Variable, VariableManager},
};

pub struct FunctionExpr {
    name: String,
    args: Vec<String>,
    f: Box<Expr>,
}
impl FunctionExpr {
    pub fn to_functions<'e>(&self, gen: &'e VariableManager) -> Option<FunctionDeclare<'e>> {
        let mut vars = HashMap::new();
        let args = self
            .args
            .iter()
            .map(|v| {
                let var = gen.new_var(v.clone());
                vars.insert(v.clone(), var.clone());
                var
            })
            .collect::<Vec<_>>();
        self.f.to_functions(gen, &vars).map(|f| FunctionDeclare {
            name: self.name.clone(),
            args,
            body: f,
        })
    }
}

pub fn parse<'e>(source: &str) -> Result<FunctionExpr, impl std::error::Error> {
    function().parse(source).map(|x| x.0)
}

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
    ) -> Option<LinearPolynomial<Variable<'e>, BigInt>> {
        match self {
            Self::Int { v } => Some(LinearPolynomial::from(BigInt::from(v.clone()))),
            Self::Variable { name } => {
                if let Some(v) = vars.get(name) {
                    Some(LinearPolynomial::from([(Some(v.clone()), BigInt::one())]))
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

    fn to_condition<'e>(
        &self,
        gen: &'e VariableManager,
        vars: &HashMap<String, Variable<'e>>,
    ) -> Option<Condition<'e>> {
        match self {
            Self::IsDivisor { l, r } => {
                if let (Some(l), Some(r)) = (l.to_int(gen, vars), r.to_linear_polynomial(gen, vars))
                {
                    Some(Condition::IsDivisor(IsDivisor::new(r, l)))
                } else {
                    None
                }
            }
            Self::LessEq { l, r } => {
                if let (Some(l), Some(r)) = (
                    l.to_linear_polynomial(gen, vars),
                    r.to_linear_polynomial(gen, vars),
                ) {
                    Some(Condition::IsNotNeg(IsNotNeg::new(r - l)))
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
            Self::Sum { v, l, r, f } => {
                if let (Some(l), Some(r)) = (l.to_functions(gen, vars), r.to_functions(gen, vars)) {
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
                    (cond.to_condition(gen, vars), f.to_functions(gen, vars))
                {
                    Some(Function::new_if(cond, f))
                } else {
                    None
                }
            }
            _ => None,
        }
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

fn function<I>() -> impl Parser<I, Output = FunctionExpr>
where
    I: Stream<Token = char>,
{
    (
        spaces(),
        variable_token(),
        op("("),
        sep_by(variable_token(), op(",")),
        op(")"),
        op("="),
        expr(),
        eof(),
    )
        .map(|t| FunctionExpr {
            name: t.1,
            args: t.3,
            f: Box::new(t.6),
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

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn parse_extra_space() {
        assert!(parse(&"f()=1").is_ok());
        assert!(parse(&" f()=1").is_ok());
        assert!(parse(&"f()=1 ").is_ok());
        assert!(parse(&" f()=1 ").is_ok());
    }
    #[test]
    fn parse_extra_token() {
        assert!(parse(&"f()=1 a").is_err());
    }
}
