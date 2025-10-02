use dashmap::DashMap;
use fxhash::{FxBuildHasher, FxHashMap, FxHashSet};
use indexmap::IndexSet;

pub type StrHashSet<'a> = FxHashSet<&'a str>;
pub type StrHashMap<'a, T> = FxHashMap<&'a str, T>;

pub type StrIndexSet<'a> = IndexSet<&'a str, FxBuildHasher>;

pub type StrDashMap<'a, T> = DashMap<&'a str, T, FxBuildHasher>;
