use crate::parser::Rule;
use crate::types::StrHashMap;
use crate::parser::comp::Edge;
use crate::edit_distance::find_best_match_for_name;

#[inline]
pub fn did_you_mean_compiled<'a>(
    target: &'a str,
    edges: &StrHashMap::<'a, Edge>,
    rules: &StrHashMap::<'a, Rule>
) -> Option::<String> {
    let candidates = edges.keys()
        .chain(rules.keys())
        .cloned()
        .collect::<Vec::<_>>();

    find_best_match_for_name(
        &candidates,
        target,
        Some(3)
    ).map(ToOwned::to_owned)
}
