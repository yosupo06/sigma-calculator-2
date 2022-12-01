use function::Function;
use optimizers::{
    loop_optimizer::LoopIfSumOptimizer, ChainedOptimizer, ConstantOptimizer, FullyOptimizer,
    ObviousBinOpOptimizer, ObviousIfOptimizer, Optimizer, PolynomialOptimizer,
    SimplifyConditionOptimizer,
};
use parser::parse;
use printer::cpp_print;
use variable::VariableManager;
use wasm_bindgen::prelude::wasm_bindgen;

#[macro_use]
extern crate combine;

pub mod constant;
pub mod eval;
pub mod function;
pub mod math;
pub mod optimizers;
pub mod parser;
pub mod polynomial;
pub mod printer;
pub mod variable;

fn default_optimize(f: Function) -> Function {
    let optimizer = ChainedOptimizer {
        optimizers: vec![
            Box::new(ConstantOptimizer {}),
            Box::new(ObviousBinOpOptimizer {}),
            Box::new(ObviousIfOptimizer {}),
            Box::new(SimplifyConditionOptimizer {}),
            Box::new(PolynomialOptimizer {}),
            Box::new(LoopIfSumOptimizer {}),
        ],
    };
    let optimizer = FullyOptimizer {
        optimizer: optimizer,
    };

    optimizer.optimize(&f).unwrap_or(f)
}

#[wasm_bindgen]
pub fn to_cpp_code(source: &str) -> String {
    let var_manager = VariableManager::new();

    let f = parse(source, &var_manager);

    if f.is_none() {
        panic!("Failed to parse");
    }
    let f = f.unwrap();

    let f = default_optimize(f);

    cpp_print(&f)
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use crate::{
        constant::Constant, eval::eval_function, function::Function, parser::parse,
        variable::VariableManager,
    };

    use crate::default_optimize;
    use num::BigInt;

    fn test_eval(f: &Function, vals: &Vec<BigInt>, expect: &Constant) {
        let val = eval_function(f, vals);
        assert_eq!(&val, expect);
    }

    fn test_function(s: &str, vals: Vec<BigInt>, expect: Constant) {
        let var_manager = VariableManager::new();
        let f = parse(s, &var_manager);
        assert!(f.is_some());
        let f = f.unwrap();

        test_eval(&f, &vals, &expect);
        let f = default_optimize(f);
        test_eval(&f, &vals, &expect);
    }

    fn test_opt_function(s: &str, vals: Vec<BigInt>, expect: Constant) {
        let var_manager = VariableManager::new();
        let f = parse(s, &var_manager);
        assert!(f.is_some());
        let f = f.unwrap();

        let f = default_optimize(f);
        test_eval(&f, &vals, &expect);
    }

    fn test_compare_opt(s: &str, vals: Vec<BigInt>) {
        let var_manager = VariableManager::new();
        let f = parse(s, &var_manager);
        assert!(f.is_some());
        let f = f.unwrap();

        let expect = eval_function(&f, &vals);

        let f = default_optimize(f);
        test_eval(&f, &vals, &expect);
    }

    #[test]
    fn test() {
        test_function("f()=123", vec![], Constant::Integer(123.into()));
        test_function(
            "f(n)=$(i=0..n)i",
            vec![100.into()],
            Constant::Integer(5050.into()),
        );
        test_function(
            "f(A, B)=$(i=A..B)[2|i]1",
            vec![2.into(), 99.into()],
            Constant::Integer(49.into()),
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
    fn test_abc269_f() {
        let s = "f(M, A, B, C, D)=$(i=A..B)$(j=C..D)[2|i+j]((i-1)*M+j)";
        let test = |m: BigInt, a: BigInt, b: BigInt, c: BigInt, d: BigInt, ans: BigInt| {
            test_opt_function(s, vec![m, a, b, c, d], Constant::Integer(ans));
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
