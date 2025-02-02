use crate::parser::Processed;
use crate::cr::DefaultTarget;
use crate::types::{StrHashMap, StrHashSet};

use std::fs;
use std::sync::Arc;
use std::collections::VecDeque;

pub type Graph<'a> = StrHashMap::<'a, Arc::<StrHashSet<'a>>>;
pub type TransitiveDeps<'a> = StrHashMap::<'a, Arc::<StrHashSet<'a>>>;

pub fn build_dependency_graph<'a>(parsed: &'a Processed) -> (Graph<'a>, DefaultTarget<'a>, TransitiveDeps<'a>) {
    fn collect_deps<'a>(
        node: &'a str,
        parsed: &'a Processed,
        graph: &mut Graph<'a>,
        visited: &mut StrHashSet<'a>,
        transitive_deps: &mut TransitiveDeps<'a>
    ) -> Arc::<StrHashSet<'a>> {
        if visited.contains(node) {
            return transitive_deps.get(node).cloned().unwrap_or_default();
        }

        visited.insert(node);

        let mut deps = parsed.jobs.get(node).map(|job| {
            job.inputs.iter()
                .chain(job.deps.iter())
                .cloned()
                .collect::<StrHashSet>()
        }).unwrap_or_default();

        if let Some(job) = parsed.jobs.get(node) {
            if let Some(rule) = parsed.rules.get(job.rule) {
                if let Some(ref depfile_template) = rule.depfile {
                    let depfile_path = match depfile_template.compile_(job, &parsed.defs) {
                        Ok(ok) => ok,
                        Err(e) => report!(job.loc, "{e}")
                    };
                    if let Ok(depfile) = fs::read_to_string(&depfile_path) {
                        let depfile = Box::leak(depfile.into_boxed_str());
                        let colon_idx = depfile.find(':');
                        let colon_idx = {
                            #[cfg(feature = "dbg")] {
                                colon_idx.unwrap()
                            }
                            #[cfg(not(feature = "dbg"))] unsafe {
                                colon_idx.unwrap_unchecked()
                            }
                        };
                        let depfile_deps = depfile[colon_idx + 1..]
                            .split_ascii_whitespace()
                            .filter(|f| *f != "\\");
                        deps.extend(depfile_deps);
                    }
                }
            }
        }

        #[inline(always)]
        fn is_system_header(path: &str) -> bool {
            path.starts_with("/usr/include/") || path.starts_with("/usr/lib/")
        }

        let transitive = deps.iter()
            .filter(|dep| !is_system_header(dep))
            .fold(Vec::with_capacity(deps.len()), |mut transitive, dep|
        {
            let deps = collect_deps(&dep, parsed, graph, visited, transitive_deps);
            transitive.extend(deps.iter().map(|h| *h));
            transitive
        });

        deps.extend(transitive);

        let deps = Arc::new(deps);

        {
            graph.insert(node, Arc::clone(&deps));
            transitive_deps.insert(node, Arc::clone(&deps));
        }

        deps
    }

    let n = parsed.jobs.len();
    let mut graph = StrHashMap::with_capacity(n);
    let mut visited = StrHashSet::with_capacity(n);
    let mut transitive_deps = StrHashMap::with_capacity(n);

    for target in parsed.jobs.keys() {
        collect_deps(target, parsed, &mut graph, &mut visited, &mut transitive_deps);
    }

    let mut reverse_graph = StrHashMap::with_capacity(n);
    for (node, deps) in graph.iter() {
        for dep in deps.iter() {
            reverse_graph.entry(*dep).or_insert_with(StrHashSet::default).insert(node);
        }
    }

    let default_target = parsed.jobs.keys()
        .find(|job| !reverse_graph.contains_key(*job))
        .cloned();

    (graph, default_target, transitive_deps)
}

pub fn topological_sort_levels<'a>(graph: &Graph<'a>) -> Vec::<Vec::<&'a str>> {
    let mut levels = Vec::new();
    let mut in_degree = StrHashMap::<i64>::with_capacity(graph.len());

    for (node, deps) in graph.iter() {
        in_degree.entry(node).or_insert(0);
        for dep in deps.iter() {
            *in_degree.entry(*dep).or_insert(0) += 1
        }
    }

    let mut queue = in_degree.iter()
        .filter(|(.., degree)| **degree == 0)
        .map(|(node, ..)| *node)
        .collect::<VecDeque::<_>>();

    while !queue.is_empty() {
        let n = queue.len();
        let mut curr_level = Vec::with_capacity(n);
        for _ in 0..n {
            let node = unsafe { queue.pop_front().unwrap_unchecked() };
            curr_level.push(node);

            let Some(deps) = graph.get(node) else { continue };
            for dep in deps.iter() {
                let e = unsafe { in_degree.get_mut(&*dep).unwrap_unchecked() };
                *e -= 1;
                if *e == 0 {
                    queue.push_back(*dep)
                }
            }
        }

        levels.push(curr_level)
    }

    if cfg!(feature = "dbg") && in_degree.values().any(|d| d.is_positive()) {
        panic!("[FATAL] cycle has been detected in the dependency graph")
    }

    levels.reverse();
    levels
}
