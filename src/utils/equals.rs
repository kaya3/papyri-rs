//! This module contains functions for comparing data structures by value.

use indexmap::IndexMap;

/// Determines whether two slices are equal by value, using the given `eq`
/// function to compare elements.
pub fn equal_lists<T>(v1: &[T], v2: &[T], eq: impl Fn(&T, &T) -> bool) -> bool {
    v1.len() == v2.len()
        && v1.iter()
            .zip(v2.iter())
            .all(|(x1, x2)| eq(x1, x2))
}

/// Determines whether two maps are equal by value, using the given `eq`
/// function to compare elements. The ordering of the `IndexMap` is not
/// considered.
pub fn equal_maps<K: std::hash::Hash + Eq, V>(v1: &IndexMap<K, V>, v2: &IndexMap<K, V>, eq: impl Fn(&V, &V) -> bool) -> bool {
    v1.len() == v2.len()
        && v1.iter()
            .all(|(k, x1)| v2.get(k).map_or(false, |x2| eq(x1, x2)))
}
