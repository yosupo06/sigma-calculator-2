use std::{char, collections::HashMap, error::Error, fmt, str::FromStr};

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

#[derive(Debug, Clone)]
pub struct ConvertError(String);

impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "converting error from expr -> function: {}", self.0)
    }
}

impl Error for ConvertError {}

pub struct FunctionExpr {
    name: String,
    args: Vec<String>,
    f: Box<Expr>,
}
impl FunctionExpr {
    pub fn to_functions<'e>(
        &self,
        gen: &'e VariableManager,
    ) -> Result<FunctionDeclare<'e>, ConvertError> {
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
    Eq {
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
    ) -> Result<LinearPolynomial<Variable<'e>, BigInt>, ConvertError> {
        match self {
            Self::Int { v } => Ok(LinearPolynomial::from(v.clone())),
            Self::Variable { name } => {
                if let Some(v) = vars.get(name) {
                    Ok(LinearPolynomial::from([(Some(v.clone()), BigInt::one())]))
                } else {
                    Err(ConvertError(format!("unknown variable: {}", name)))
                }
            }
            Self::Add { l, r } => {
                let (l, r) = (
                    l.to_linear_polynomial(gen, vars)?,
                    r.to_linear_polynomial(gen, vars)?,
                );
                Ok(l + r)
            }
            Self::Sub { l, r } => {
                let (l, r) = (
                    l.to_linear_polynomial(gen, vars)?,
                    r.to_linear_polynomial(gen, vars)?,
                );
                Ok(l - r)
            }
            Self::Mul { l, r } => {
                let (l, r) = (
                    l.to_linear_polynomial(gen, vars)?,
                    r.to_linear_polynomial(gen, vars)?,
                );

                if let Some(l) = l.to_constant() {
                    Ok(r * l)
                } else if let Some(r) = r.to_constant() {
                    Ok(l * r)
                } else {
                    Err(ConvertError(
                        "(linear pol) * (linear pol) might not be (linear pol)".to_string(),
                    ))
                }
            }
            Self::Neg { v } => Ok(-(v.to_linear_polynomial(gen, vars)?)),
            _ => Err(ConvertError(
                "invalid expr for linear polynomial".to_string(),
            )),
        }
    }

    fn to_condition<'e>(
        &self,
        gen: &'e VariableManager,
        vars: &HashMap<String, Variable<'e>>,
    ) -> Result<Condition<'e>, ConvertError> {
        match self {
            Self::IsDivisor { l, r } => {
                let Some(l) = l.to_int(gen, vars) else {
                    return Err(ConvertError("left of is_divisor must be constant".to_string()));
                };
                let r = r.to_linear_polynomial(gen, vars)?;
                Ok(Condition::IsDivisor(IsDivisor::new(r, l)))
            }
            Self::LessEq { l, r } => {
                let (l, r) = (
                    l.to_linear_polynomial(gen, vars)?,
                    r.to_linear_polynomial(gen, vars)?,
                );
                Ok(Condition::IsNotNeg(IsNotNeg::new(r - l)))
            }
            _ => Err(ConvertError("invalid expr for condition".to_string())),
        }
    }

    fn to_functions<'e>(
        &self,
        gen: &'e VariableManager,
        vars: &HashMap<String, Variable<'e>>,
    ) -> Result<Function<'e>, ConvertError> {
        match self {
            Self::Int { v } => Ok(Function::new_polynomial_as_int(Polynomial::from(
                BigRational::from(v.clone()),
            ))),
            Self::Variable { name } => {
                if let Some(v) = vars.get(name) {
                    Ok(Function::new_polynomial_as_int(Polynomial::from([(
                        v.clone().into(),
                        BigRational::one(),
                    )])))
                } else {
                    Err(ConvertError(format!("unknown variable: {}", name)))
                }
            }
            Self::Add { l, r } => Ok(Function::new_add(
                l.to_functions(gen, vars)?,
                r.to_functions(gen, vars)?,
            )),
            Self::Sub { l, r } => Ok(Function::new_add(
                l.to_functions(gen, vars)?,
                Function::new_neg(r.to_functions(gen, vars)?),
            )),
            Self::Mul { l, r } => Ok(Function::new_mul(
                l.to_functions(gen, vars)?,
                r.to_functions(gen, vars)?,
            )),
            Self::Neg { v } => Ok(Function::new_neg(v.to_functions(gen, vars)?)),
            Self::Sum { v, l, r, f } => {
                let l = l.to_functions(gen, vars)?;
                let r = r.to_functions(gen, vars)?;
                let mut vars = vars.clone();
                let var = gen.new_var(v.clone());
                vars.insert(v.clone(), var.clone());
                let f = f.to_functions(gen, &vars)?;

                Ok(Function::new_loop_sum(var, l, r, f))
            }
            Self::If { cond, f } => {
                let f = f.to_functions(gen, vars)?;

                if let Self::Eq { l, r } = cond.as_ref() {
                    let l = l.to_linear_polynomial(gen, vars)?;
                    let r = r.to_linear_polynomial(gen, vars)?;
                    return Ok(Function::new_if(
                        Condition::IsNotNeg(IsNotNeg::new(r.clone() - l.clone())),
                        Function::new_if(Condition::IsNotNeg(IsNotNeg::new(l - r)), f),
                    ));
                }
                Ok(Function::new_if(cond.to_condition(gen, vars)?, f))
            }
            _ => Err(ConvertError("invalid expr for function".to_string())),
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
            op("="),
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
                    "=" => Expr::Eq { l: Box::new(l), r: Box::new(r) },
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
