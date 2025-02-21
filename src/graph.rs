use crate::parser::comp::Phony;
use crate::dbg_unwrap::DbgUnwrap;
use crate::parser::{Compiled, DefaultEdge};
use crate::types::{StrHashMap, StrHashSet, StrIndexSet};

use std::fs;
use std::sync::Arc;
use std::collections::VecDeque;

use bumpalo::Bump;
#[cfg(feature = "dbg")]
use tramer::tramer;

pub type Levels<'a> = Vec::<Vec::<&'a str>>;
pub type Graph<'a> = StrHashMap::<'a, Arc::<StrIndexSet<'a>>>;

#[cfg_attr(feature = "dbg", tramer("nanos"))]
pub fn build_dependency_graph<'a>(
    arena: &'a Bump,
    processed: &'a Compiled,
    default_edge: DefaultEdge<'a>
) -> (Graph<'a>, DefaultEdge<'a>, Graph<'a>) {
    fn collect_deps<'a>(
        node: &'a str,
        arena: &'a Bump,
        parsed: &'a Compiled,
        graph: &mut Graph<'a>,
        visited: &mut StrHashSet<'a>,
        transitive_deps: &mut Graph<'a>
    ) -> Arc::<StrIndexSet<'a>> {
        if visited.contains(node) {
            return transitive_deps.get(node).cloned().unwrap_or_default();
        }

        visited.insert(node);

        let mut deps = parsed.edges.get(node).map(|edge| {
            match &edge.phony {
                Phony::Phony { .. } => { StrIndexSet::default() },
                Phony::NotPhony { deps, inputs, .. } => {
                    inputs.iter()
                        .chain(deps.iter())
                        .cloned()
                        .collect::<StrIndexSet>()
                } 
            }
        }).unwrap_or_default();

        // check if depfile exists, if it does => read it, extend our deps with the ones that are listed in the .d file
        if let Some(edge) = parsed.edges.get(node) {
            if let Some(depfile_path) = edge.depfile() {
                if let Ok(depfile) = fs::read_to_string(depfile_path) {
                    let depfile = arena.alloc_str(&depfile);
                    let colon_idx = depfile.find(':').unwrap_dbg();
                    let depfile_deps = depfile[colon_idx + 1..]
                        .split_ascii_whitespace()
                        .filter(|f| *f != "\\");

                    deps.extend(depfile_deps);
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
            let deps = collect_deps(&dep, arena, parsed, graph, visited, transitive_deps);
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

    let n = processed.edges.len();
    let mut graph = Graph::with_capacity(n);
    let mut visited = StrHashSet::with_capacity(n);
    let mut transitive_deps = Graph::with_capacity(n);

    for target in processed.edges.keys() {
        collect_deps(target, arena, processed, &mut graph, &mut visited, &mut transitive_deps);
    }

    let default_edge = default_edge.or({
        if let Some(ref dt) = processed.default_target {
            let edge = processed.edges.get(dt.as_str());
            debug_assert!(edge.is_some());
            edge
        } else if graph.is_empty() {
            let edge = processed.edges.values().next();
            debug_assert!(edge.is_some());
            edge
        } else {
            let mut reverse_graph = StrHashMap::with_capacity(n);
            for (node, deps) in graph.iter() {
                for dep in deps.iter() {
                    reverse_graph.entry(*dep).or_insert_with(StrHashSet::default).insert(node);
                }
            }

            // find the edges that do not act as an input anywhere,
            // then sort those by their first appearance in the source code row-wise
            processed.edges.keys()
                .filter(|edge| !reverse_graph.contains_key(*edge))
                .map(|t| unsafe { processed.edges.get(t).unwrap_unchecked() })
                .min_by(|x, y| x.loc.0.cmp(&y.loc.0))
        }
    });

    (graph, default_edge, transitive_deps)
}

pub fn topological_sort<'a>(graph: &Graph<'a>) -> Levels<'a> {
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
                let e = unsafe { in_degree.get_mut(dep).unwrap_unchecked() };
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
