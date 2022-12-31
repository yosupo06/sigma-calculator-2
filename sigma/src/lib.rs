use std::error::Error;

use function::FunctionDeclare;
use optimizers::{
    binop_optimize_rule, constant_optimize_rule, fully_optimize,
    loop_optimizer::loop_if_sum_optimize_rule, obvious_if_optimize_rule, polynomial_optimize_rule,
    OptimizeRule,
};
use parser::parse;
use printer::cpp_print;
use variable::VariableManager;
use wasm_bindgen::{prelude::wasm_bindgen, JsError};

#[macro_use]
extern crate combine;

pub mod eval;
pub mod function;
pub mod math;
pub mod optimizers;
pub mod parser;
pub mod polynomials;
pub mod printer;
pub mod variable;

fn default_optimize<'e>(f: FunctionDeclare<'e>) -> FunctionDeclare<'e> {
    fully_optimize(
        f,
        constant_optimize_rule()
            .or(binop_optimize_rule())
            .or(polynomial_optimize_rule())
            .or(obvious_if_optimize_rule())
            .or(loop_if_sum_optimize_rule()),
    )
}

pub fn to_function<'e>(
    source: &str,
    var_manager: &'e VariableManager,
) -> Result<FunctionDeclare<'e>, Box<dyn Error>> {
    let expr = parse(source)?;
    Ok(expr.to_functions(&var_manager)?)
}

#[wasm_bindgen]
pub fn to_cpp_code(source: &str) -> Result<String, JsError> {
    let expr = parse(source)?;
    let var_manager = VariableManager::default();
    let f = expr.to_functions(&var_manager)?;

    let f = default_optimize(f);

    Ok(cpp_print(&f))
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use crate::function::FunctionDeclare;
    use crate::{eval::eval_function, variable::VariableManager};

    use crate::{default_optimize, to_function};
    use num::BigInt;

    fn test_eval(f: &FunctionDeclare, vals: &Vec<BigInt>, expect: &BigInt) {
        let val = eval_function(f, vals);
        assert_eq!(&val, expect);
    }

    fn test_function(s: &str, vals: Vec<BigInt>, expect: BigInt) {
        let var_manager = VariableManager::default();
        let f = to_function(s, &var_manager);
        let f = f.unwrap();

        test_eval(&f, &vals, &expect);
        let f = default_optimize(f);
        test_eval(&f, &vals, &expect);
    }

    fn test_opt_function(s: &str, vals: Vec<BigInt>, expect: BigInt) {
        let var_manager = VariableManager::default();
        let f = to_function(s, &var_manager);
        let f = f.unwrap();

        let f = default_optimize(f);
        test_eval(&f, &vals, &expect);
    }

    fn test_compare_opt(s: &str, vals: Vec<BigInt>) {
        let var_manager = VariableManager::default();
        let f = to_function(s, &var_manager);
        let f = f.unwrap();

        let expect = eval_function(&f, &vals);

        let f = default_optimize(f);
        test_eval(&f, &vals, &expect);
    }

    #[test]
    fn test() {
        test_function("f()=123", vec![], 123.into());
        test_function("f(n)=$(i=0..n)i", vec![100.into()], 5050.into());
        test_function(
            "f(A, B)=$(i=A..B)[2|i]1",
            vec![2.into(), 99.into()],
            49.into(),
        );
    }

    #[test]
    fn stress_even_count() {
        for a in 0..10 {
            for b in 0..10 {
                test_compare_opt("f(A, B)=$(i=A..B)[2|i]1", vec![a.into(), b.into()]);
            }
        }
    }
    #[test]
    fn stress1() {
        for a in 0..10 {
            for b in 0..10 {
                for c in 0..10 {
                    test_compare_opt(
                        "f(A, B, C)=$(i=A..B)[2*C-i>=0][C-2*i>=0]1",
                        vec![a.into(), b.into(), c.into()],
                    );
                }
            }
        }
    }

    #[test]
    fn stress2() {
        for a in 0..10 {
            for b in 0..10 {
                test_compare_opt("f(A, B)=$(i=2*A..3*B)1", vec![a.into(), b.into()]);
            }
        }
    }

    #[test]
    fn stress3() {
        for a in 0..10 {
            for b in 0..10 {
                for c in 0..10 {
                    test_compare_opt(
                        "f(A, B, C)=$(i=A..B)[2*C-3*i>=0][4*i-5*C>=0]1",
                        vec![a.into(), b.into(), c.into()],
                    );
                }
            }
        }
    }

    #[test]
    fn stress4() {
        for a in 0..10 {
            for b in 0..10 {
                for c in 0..10 {
                    test_compare_opt(
                        "f(A, B, C)=$(i=A..B)[2*C-3*i=0][4*i-5*C=0]1",
                        vec![a.into(), b.into(), c.into()],
                    );
                }
            }
        }
    }

    #[test]
    fn test_abc269_f() {
        let s = "f(M, A, B, C, D)=$(i=A..B)$(j=C..D)[2|i+j]((i-1)*M+j)";
        let test = |m: BigInt, a: BigInt, b: BigInt, c: BigInt, d: BigInt, ans: BigInt| {
            test_opt_function(s, vec![m, a, b, c, d], ans);
        };
        test(
            999999999.into(),
            999999999.into(),
            999999999.into(),
            999999999.into(),
            999999999.into(),
            BigInt::from_str("999999998000000001").unwrap(),
        );
        test(
            999999999.into(),
            216499784.into(),
            840031647.into(),
            84657913.into(),
            415448790.into(),
            BigInt::from_str("54479687255635346501508556066356552").unwrap(),
        );
        test(
            999999999.into(),
            1.into(),
            999999999.into(),
            1.into(),
            999999999.into(),
            BigInt::from_str("249999999000000001999999998000000001").unwrap(),
        );
    }
}
