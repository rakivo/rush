use std::{mem, cmp};

//
// this code is derived from the Rust compiler (`rustc`), specifically,
// this file: https://github.com/rust-lang/rust/blob/3b022d8ceea570db9730be34d964f0cc663a567f/compiler/rustc_span/src/edit_distance.rs.
//
// (`rustc`) is dual-licensed under the MIT and Apache 2.0 licenses. See the LICENSE
// file in the Rust repository for more details.
//

/// Finds the [edit distance] between two strings.
///
/// Returns `None` if the distance exceeds the limit.
///
/// [edit distance]: https://en.wikipedia.org/wiki/Edit_distance
fn edit_distance(a: &str, b: &str, limit: usize) -> Option<usize> {
    let mut a = &a.chars().collect::<Vec<_>>()[..];
    let mut b = &b.chars().collect::<Vec<_>>()[..];

    // Ensure that `b` is the shorter string, minimizing memory use.
    if a.len() < b.len() {
        mem::swap(&mut a, &mut b);
    }

    let min_dist = a.len() - b.len();
    // If we know the limit will be exceeded, we can return early.
    if min_dist > limit {
        return None;
    }

    // Strip common prefix.
    while let Some(((b_char, b_rest), (a_char, a_rest))) = b.split_first().zip(a.split_first()) {
        if a_char != b_char { break }
        a = a_rest;
        b = b_rest;
    }
    // Strip common suffix.
    while let Some(((b_char, b_rest), (a_char, a_rest))) = b.split_last().zip(a.split_last()) {
        if a_char != b_char { break }
        a = a_rest;
        b = b_rest;
    }

    // If either string is empty, the distance is the length of the other.
    // We know that `b` is the shorter string, so we don't need to check `a`.
    if b.len() == 0 {
        return Some(min_dist);
    }

    let mut prev_prev = vec![usize::MAX; b.len() + 1];
    let mut prev = (0..=b.len()).collect::<Vec<_>>();
    let mut current = vec![0; b.len() + 1];

    // row by row
    for i in 1..=a.len() {
        current[0] = i;
        let a_idx = i - 1;

        // column by column
        for j in 1..=b.len() {
            let b_idx = j - 1;

            // There is no cost to substitute a character with itself.
            let substitution_cost = if a[a_idx] == b[b_idx] { 0 } else { 1 };

            current[j] = cmp::min(
                // deletion
                prev[j] + 1,
                cmp::min(
                    // insertion
                    current[j - 1] + 1,
                    // substitution
                    prev[j - 1] + substitution_cost,
                ),
            );

            if (i > 1) && (j > 1) && (a[a_idx] == b[b_idx - 1]) && (a[a_idx - 1] == b[b_idx]) {
                // transposition
                current[j] = cmp::min(current[j], prev_prev[j - 2] + 1);
            }
        }

        // Rotate the buffers, reusing the memory.
        [prev_prev, prev, current] = [prev, current, prev_prev];
    }

    // `prev` because we already rotated the buffers.
    let distance = prev[b.len()];
    (distance <= limit).then_some(distance)
}


fn edit_distance_with_substrings(a: &str, b: &str, limit: usize) -> Option<usize> {
    let n = a.chars().count();
    let m = b.chars().count();

    // Check one isn't less than half the length of the other. If this is true then there is a
    // big difference in length.
    let big_len_diff = (n * 2) < m || (m * 2) < n;
    let len_diff = if n < m { m - n } else { n - m };
    let distance = edit_distance(a, b, limit + len_diff)?;

    // This is the crux, subtracting length difference means exact substring matches will now be 0
    let score = distance - len_diff;

    // If the score is 0 but the words have different lengths then it's a substring match not a full
    // word match
    let score = if score == 0 && len_diff > 0 && !big_len_diff {
        1 // Exact substring match, but not a total word match so return non-zero
    } else if !big_len_diff {
        // Not a big difference in length, discount cost of length difference
        score + (len_diff + 1) / 2
    } else {
        // A big difference in length, add back the difference in length to the score
        score + len_diff
    };

    (score <= limit).then_some(score)
}

#[cold]
fn find_best_match_for_name_impl<'a>(
    use_substring_score: bool,
    candidates: &[&'a str],
    lookup: &'a str,
    dist: Option<usize>,
) -> Option<&'a str> {
    let lookup_uppercase = lookup.to_uppercase();

    // Priority of matches:
    // 1. Exact case insensitive match or Substring insensitive match
    // 2. Edit distance match
    // 3. Sorted word match
    if let Some(c) = candidates.iter().find(|c| {
        c.to_uppercase() == lookup_uppercase || c.to_uppercase().contains(&lookup_uppercase)
    }) {
        return Some(*c);
    }

    // `fn edit_distance()` use `chars()` to calculate edit distance, so we must
    // also use `chars()` (and not `str::len()`) to calculate length here.
    let lookup_len = lookup.chars().count();

    let mut dist = dist.unwrap_or_else(|| cmp::max(lookup_len, 3) / 3);
    let mut best = None;
    // store the candidates with the same distance, only for `use_substring_score` current.
    let mut next_candidates = vec![];
    for c in candidates {
        match if use_substring_score {
            edit_distance_with_substrings(lookup, c, dist)
        } else {
            edit_distance(lookup, c, dist)
        } {
            Some(0) => return Some(*c),
            Some(d) => {
                if use_substring_score {
                    if d < dist {
                        dist = d;
                        next_candidates.clear();
                    } else {
                        // `d == dist` here, we need to store the candidates with the same distance
                        // so we won't decrease the distance in the next loop.
                    }
                    next_candidates.push(*c);
                } else {
                    dist = d - 1;
                }
                best = Some(*c);
            }
            None => {}
        }
    }

    // We have a tie among several candidates, try to select the best among them ignoring substrings.
    // For example, the candidates list `force_capture`, `capture`, and user inputted `forced_capture`,
    // we select `force_capture` with a extra round of edit distance calculation.
    if next_candidates.len() > 1 {
        debug_assert!(use_substring_score);
        best = find_best_match_for_name_impl(
            false,
            &next_candidates,
            lookup,
            Some(lookup.len()),
        );
    }
    if best.is_some() {
        return best;
    }

    find_match_by_sorted_words(candidates, lookup)
}

fn find_match_by_sorted_words<'a>(iter_names: &[&'a str], lookup: &str) -> Option<&'a str> {
    let lookup_sorted_by_words = sort_by_words(lookup);
    iter_names.iter().fold(None, |result, candidate| {
        if sort_by_words(candidate) == lookup_sorted_by_words {
            Some(*candidate)
        } else {
            result
        }
    })
}

pub fn find_best_match_for_name<'a>(
    candidates: &[&'a str],
    lookup: &'a str,
    dist: Option<usize>,
) -> Option<&'a str> {
    find_best_match_for_name_impl(false, candidates, lookup, dist)
}

fn sort_by_words(name: &str) -> Vec<&str> {
    let mut split_words: Vec<&str> = name.split('_').collect();
    // We are sorting primitive &strs and can use unstable sort here.
    split_words.sort_unstable();
    split_words
}
