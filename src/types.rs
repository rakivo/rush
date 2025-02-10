use dashmap::DashMap;
use fxhash::{FxHashMap, FxHashSet, FxBuildHasher};

pub type StrHashSet<'a> = FxHashSet::<&'a str>;
pub type StrHashMap<'a, T> = FxHashMap::<&'a str, T>;

pub type StrDashMap<'a, T> = DashMap::<&'a str, T, FxBuildHasher>;
