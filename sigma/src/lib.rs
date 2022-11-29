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
