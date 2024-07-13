use std::collections::BTreeMap;

pub fn extract_if<'map, K: Ord + Copy, V>(
    map: &'map mut BTreeMap<K, V>,
    mut predicate: impl FnMut(&V) -> bool + 'map,
) -> impl Iterator<Item = (K, V)> + 'map {
    std::iter::from_fn(move || {
        let (&k, _) = map.iter().find(|(_, v)| predicate(v))?;
        map.remove_entry(&k)
    })
}
