use crate::function::{Function, FunctionData};

fn replace_children<'e, F>(f: &Function<'e>, rule: &mut F) -> Option<Function<'e>>
where
    F: FnMut(&Function<'e>) -> Option<Function<'e>>,
{
    match f.data() {
        FunctionData::Polynomial { p: _ } => None,
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
        FunctionData::If { cond, f, .. } => {
            let f2 = replace_all_internal(f, rule);

            if f2.is_some() {
                Some(Function::new_if(cond.clone(), f2.unwrap_or(f.clone())))
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
        FunctionData::Neg { v } => replace_all_internal(v, rule).map(Function::new_neg),
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
