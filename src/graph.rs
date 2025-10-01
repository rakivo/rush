use crate::dbg_unwrap::DbgUnwrap;
use crate::parser::comp::{self, Phony};
use crate::parser::Compiled;
use crate::types::{StrHashMap, StrHashSet, StrIndexSet};
use crate::util;

use std::collections::VecDeque;
use std::sync::Arc;

use bumpalo::Bump;
#[cfg(feature = "dbg")]
use tramer::tramer;

pub type DefaultEdge<'a> = Option<&'a comp::Edge<'a>>;

pub type Levels<'a> = Vec<Vec<&'a str>>;
pub type Graph<'a> = StrHashMap<'a, Arc<StrIndexSet<'a>>>;

#[cfg_attr(feature = "dbg", tramer("nanos"))]
pub fn build_dependency_graph<'a>(
    arena: &'a Bump,
    compiled: &'a Compiled,
    default_edge: DefaultEdge<'a>,
) -> (Graph<'a>, DefaultEdge<'a>, Graph<'a>) {
    fn collect_deps<'a>(
        node: &'a str,
        arena: &'a Bump,
        parsed: &'a Compiled,
        graph: &mut Graph<'a>,
        visited: &mut StrHashSet<'a>,
        transitive_deps: &mut Graph<'a>,
    ) -> Arc<StrIndexSet<'a>> {
        if visited.contains(node) {
            return transitive_deps.get(node).cloned().unwrap_or_default();
        }

        visited.insert(node);

        let mut deps = parsed
            .edges
            .get(node)
            .map(|edge| match &edge.phony {
                Phony::Phony { .. } => StrIndexSet::default(),
                Phony::NotPhony { deps, inputs, .. } => inputs
                    .iter()
                    .chain(deps.iter())
                    .cloned()
                    .collect::<StrIndexSet>(),
            })
            .unwrap_or_default();

        if let Some(edge) = parsed.edges.get(node) {
            if let Some(depfile_path) = edge.depfile() {
                if let Ok(depfile) = util::read_file_into_arena_str(arena, depfile_path) {
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

        let non_system_deps_count = deps.iter()
            .filter(|dep| !is_system_header(dep))
            .count();

        let mut transitive = Vec::with_capacity(non_system_deps_count * 2);
        for dep in deps.iter().filter(|dep| !is_system_header(dep)) {
            let deps = collect_deps(dep, arena, parsed, graph, visited, transitive_deps);
            transitive.extend(deps.iter().copied());
        }

        deps.extend(transitive);

        let deps = Arc::new(deps);

        {
            graph.insert(node, Arc::clone(&deps));
            transitive_deps.insert(node, Arc::clone(&deps));
        }

        deps
    }

    let n = compiled.edges.len();
    let mut graph = Graph::with_capacity(n);
    let mut visited = StrHashSet::with_capacity(n);
    let mut transitive_deps = Graph::with_capacity(n);

    for target in compiled.edges.keys() {
        collect_deps(
            target,
            arena,
            compiled,
            &mut graph,
            &mut visited,
            &mut transitive_deps,
        );
    }

    let default_edge = default_edge.or({
        if let Some(comp::Target { target, loc }) = compiled.default_target.as_ref() {
            let edge = compiled
                .edges
                .get(target.as_str())
                .unwrap_or_else(|| {
                    util::report_undefined_target(target, Some(loc), compiled)
                });

            Some(edge)
        } else if graph.is_empty() {
            let edge = compiled.edges.values().next();
            debug_assert!(edge.is_some());
            edge
        } else {
            let mut reverse_graph = StrHashMap::<StrHashSet>::with_capacity(n);
            for (node, deps) in graph.iter() {
                for dep in deps.iter() {
                    reverse_graph
                        .entry(*dep)
                        .or_default()
                        .insert(node);
                }
            }

            // find the edges that do not act as an input anywhere,
            // then sort those by their first appearance in the source code row-wise
            compiled
                .edges
                .keys()
                .filter(|edge| !reverse_graph.contains_key(*edge))
                .map(|t| unsafe { compiled.edges.get(t).unwrap_unchecked() })
                .min_by(|x, y| x.loc.row.cmp(&y.loc.row))
        }
    });

    (graph, default_edge, transitive_deps)
}

pub fn topological_sort<'a>(graph: &Graph<'a>, context: &Compiled) -> Levels<'a> {
    let mut levels = Vec::with_capacity(8);
    let mut parent = StrHashMap::with_capacity(graph.len());
    let mut in_degree = StrHashMap::with_capacity(graph.len());

    for (node, deps) in graph.iter() {
        in_degree.entry(node).or_insert(0);
        for dep in deps.iter().cloned() {
            *in_degree.entry(dep).or_insert(0) += 1;
            parent.entry(dep).or_insert(node);
        }
    }

    let mut queue = in_degree
        .iter()
        .filter(|(.., degree)| **degree == 0)
        .map(|(node, ..)| *node)
        .collect::<VecDeque<_>>();

    while !queue.is_empty() {
        let n = queue.len();
        let mut curr_level = Vec::with_capacity(n);
        for _ in 0..n {
            let node = unsafe { queue.pop_front().unwrap_unchecked() };
            curr_level.push(node);

            if let Some(deps) = graph.get(node) {
                for dep in deps.iter() {
                    let e = unsafe { in_degree.get_mut(dep).unwrap_unchecked() };
                    *e -= 1;
                    if *e == 0 {
                        queue.push_back(*dep)
                    }
                }
            }
        }

        levels.push(curr_level)
    }

    if let Some(cycle_node) = in_degree
        .iter()
        .find(|(.., degree)| **degree > 0)
        .map(|(node, ..)| *node)
    {
        fn reconstruct_cycle<'a>(
            start: &'a str,
            graph: &Graph<'a>,
            visited: &mut StrHashSet<'a>,
            cycle_path: &mut Vec<&'a str>,
        ) {
            let mut curr = start;
            loop {
                if visited.contains(curr) {
                    break;
                }

                visited.insert(curr);
                cycle_path.push(curr);

                let Some(next) = graph.get(curr).and_then(|deps| deps.first()) else {
                    break;
                };

                curr = next
            }
        }

        let mut cycle_path = Vec::with_capacity(32);
        reconstruct_cycle(
            cycle_node,
            graph,
            &mut StrHashSet::with_capacity(graph.len()),
            &mut cycle_path,
        );

        // the cycled path should end with the edge it began with
        if let Some(start) = cycle_path.first() {
            cycle_path.push(start)
        }

        /* report the cycle */
        {
            let edge = cycle_path[0];
            let msg = if cycle_path.len() == 2 {
                format!("edge {edge:?} depends on itself")
            } else {
                let pretty = util::pretty_print_slice(&cycle_path, " -> ");
                format! {
                    "cycle detected: {pretty}\nnote: edge {edge:?} is causing the cycle"
                }
            };

            if let Some(comp::Edge { loc, .. }) = context.edges.get(edge) {
                report_panic!(loc, "{msg}")
            } else {
                panic!("{msg}")
            }
        }
    }

    levels.reverse();
    levels
}
