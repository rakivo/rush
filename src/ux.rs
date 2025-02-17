use std::process::exit;

use crate::parser::Rule;
use crate::types::StrHashMap;
use crate::parser::comp::Job;
use crate::flags::{FLAG_STRS, MODE_STRS};

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
                eprintln!("undefined flag: {f}");
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
pub fn did_you_mean_flags(flag: &str) -> Option::<&str> {
    let candidates = FLAG_STRS.iter()
        .chain(MODE_STRS.iter())
        .cloned()
        .collect::<Vec::<_>>();

    did_you_mean(flag, &candidates, 2)
}

#[inline]
pub fn did_you_mean_compiled<'a>(
    target: &'a str,
    jobs: &StrHashMap::<'a, Job>,
    rules: &StrHashMap::<'a, Rule>
) -> Option::<&'a str> {
    let candidates = jobs.keys()
        .chain(rules.keys())
        .cloned()
        .collect::<Vec::<_>>();

    did_you_mean(target, &candidates, 3)
}

fn did_you_mean<'a>(
    target: &'a str,
    candidates: &[&'a str],
    max_distance: usize
) -> Option::<&'a str> {
    candidates.iter().fold(None, |best, candidate| {
        let d = levenshtein(candidate, target);
        if d <= max_distance {
            Some(candidate)
        } else {
            best
        }
    })
}

fn levenshtein(a: &str, b: &str) -> usize {
    let a = a.as_bytes();
    let b = b.as_bytes();

    let m = a.len();
    let n = b.len();

    let mut dp = vec![vec![0; n + 1]; m + 1];

    (0..=m).for_each(|i| dp[i][0] = i);
    (0..=n).for_each(|j| dp[0][j] = j);

    for i in 1..=m {
        for j in 1..=n {
            if a[i - 1] == b[j - 1] {
                dp[i][j] = dp[i - 1][j - 1]
            } else {
                dp[i][j] = 1 +
                         dp[i][j - 1]
                    .min(dp[i - 1][j])
                    .min(dp[i - 1][j - 1])
            }
        }
    } dp[m][n]
}
