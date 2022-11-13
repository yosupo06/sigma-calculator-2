use crate::function::{Function, FunctionData};

fn replace_children<'e, F>(f: &Function<'e>, rule: &mut F) -> Option<Function<'e>>
where
    F: FnMut(&Function<'e>) -> Option<Function<'e>>,
{
    match f.data() {
        FunctionData::Bool { f: _ } => None,
        FunctionData::PolynomialAsInt { p: _ } => None,
        FunctionData::LoopSum { i, l, r, f: cf, .. } => {
            let l2 = replace_all_internal(l, rule);
            let r2 = replace_all_internal(r, rule);
            let f2 = replace_all_internal(cf, rule);

            if l2.is_some() || r2.is_some() || f2.is_some() {
                Some(Function::new_loop_sum(
                    i.clone(),
                    l2.unwrap_or(l.clone()),
                    r2.unwrap_or(r.clone()),
                    f2.unwrap_or(cf.clone()),
                ))
            } else {
                None
            }
        }
        FunctionData::If { cond, f: cf, .. } => {
            let p2 = replace_all_internal(cond, rule);
            let cf2 = replace_all_internal(cf, rule);

            if p2.is_some() || cf2.is_some() {
                Some(Function::new_if(
                    p2.unwrap_or(cond.clone()),
                    cf2.unwrap_or(cf.clone()),
                ))
            } else {
                None
            }
        }
        FunctionData::Add { l, r, .. } => {
            let l2 = replace_all_internal(l, rule);
            let r2 = replace_all_internal(r, rule);

            if l2.is_some() || r2.is_some() {
                Some(Function::new_add(
                    l2.unwrap_or(l.clone()),
                    r2.unwrap_or(r.clone()),
                ))
            } else {
                None
            }
        }
        FunctionData::Mul { l, r, .. } => {
            let l2 = replace_all_internal(l, rule);
            let r2 = replace_all_internal(r, rule);

            if l2.is_some() || r2.is_some() {
                Some(Function::new_mul(
                    l2.unwrap_or(l.clone()),
                    r2.unwrap_or(r.clone()),
                ))
            } else {
                None
            }
        }
        FunctionData::IntIsNotNeg { p: _ } => None,
        FunctionData::IntIsDivisor { l: _, r: _ } => None,
        FunctionData::Neg { v } => replace_all_internal(v, rule).map(Function::new_neg),
        FunctionData::Declare { name, args, body } => replace_all_internal(body, rule)
            .map(|body| Function::new_declare(name.clone(), args.clone(), body)),
        _ => {
            todo!()
        }
    }
}

fn replace_all_internal<'e, F>(f: &Function<'e>, rule: &mut F) -> Option<Function<'e>>
where
    F: FnMut(&Function<'e>) -> Option<Function<'e>>,
{
    let mut f = f.clone();
    let mut changed = false;
    loop {
        if let Some(f2) = replace_children(&f, rule) {
            f = f2;
            changed = true;
        }
        if let Some(f2) = rule(&f) {
            f = f2;
            changed = true;
        } else {
            break;
        }
    }
    if changed {
        Some(f)
    } else {
        None
    }
}

pub fn replace_all<'e, F>(f: &Function<'e>, mut rule: F) -> Option<Function<'e>>
where
    F: FnMut(&Function<'e>) -> Option<Function<'e>>,
{
    replace_all_internal(f, &mut rule)
}
