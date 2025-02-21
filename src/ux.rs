use std::process::exit;

use crate::parser::Rule;
use crate::types::StrHashMap;
use crate::parser::comp::Job;
use crate::flags::{FLAG_STRS, MODE_STRS};
use crate::edit_distance::find_best_match_for_name;

pub fn check_args<'a>(args: &'a [String]) -> Option::<&'a String> {
    args.get(1)
        .filter(|f| args.len() == 2 && !MODE_STRS.contains(&f.as_str()))
        .inspect(|v| {
            if v.as_bytes().first().map_or(false, |&b| b == b'-') &&
                !FLAG_STRS.contains(&v.as_str())
            {
                eprintln!("undefined flag: {v}");
                if let Some(flag) = did_you_mean_flags(v) {
                    eprintln!("did you mean: {flag}?")
                } exit(1)
            }
        }).or(args[1..].windows(2).find(|w| {
            let f = w[0].as_str();
            let v = w[1].as_str();

            if v.as_bytes().first().map_or(false, |&b| b == b'-') &&
                !FLAG_STRS.contains(&f)
            {
                eprintln!("undefined flag: {v}");
                if let Some(flag) = did_you_mean_flags(v) {
                    eprintln!("did you mean: {flag}?")
                } exit(1)
            }

            !FLAG_STRS.contains(&f) &&
            !FLAG_STRS.contains(&v) &&
            !MODE_STRS.contains(&v)
        }).map(|w| &w[1]))
}

#[inline]
pub fn did_you_mean_flags(flag: &str) -> Option::<String> {
    let candidates = FLAG_STRS.iter()
        .chain(MODE_STRS.iter())
        .cloned()
        .collect::<Vec::<_>>();

    find_best_match_for_name(
        &candidates,
        flag,
        Some(3)
    ).map(ToOwned::to_owned)
}

#[inline]
pub fn did_you_mean_compiled<'a>(
    target: &'a str,
    jobs: &StrHashMap::<'a, Job>,
    rules: &StrHashMap::<'a, Rule>
) -> Option::<String> {
    let candidates = jobs.keys()
        .chain(rules.keys())
        .cloned()
        .collect::<Vec::<_>>();

    find_best_match_for_name(
        &candidates,
        target,
        Some(3)
    ).map(ToOwned::to_owned)
}
