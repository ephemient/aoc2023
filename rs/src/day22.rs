use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};

type Multimap<T> = BTreeMap<T, BTreeSet<T>>;

fn parse(data: &str) -> Option<(usize, Multimap<usize>, Multimap<usize>)> {
    let mut bricks = data
        .lines()
        .map(|line| {
            let (first, second) = line.split_once('~')?;
            {
                let mut iter = first.splitn(3, ',');
                Some((
                    iter.next()?.parse::<i32>().ok()?,
                    iter.next()?.parse::<i32>().ok()?,
                    iter.next()?.parse::<i32>().ok()?,
                ))
            }
            .zip({
                let mut iter = second.splitn(3, ',');
                Some((
                    iter.next()?.parse::<i32>().ok()?,
                    iter.next()?.parse::<i32>().ok()?,
                    iter.next()?.parse::<i32>().ok()?,
                ))
            })
        })
        .collect::<Option<Vec<_>>>()?;
    bricks.sort_by_key(|((_, _, z), _)| *z);
    let mut heights = BTreeMap::new();
    for ((x0, y0, z0), (x1, y1, z1)) in &mut bricks {
        let z = (*x0..=*x1)
            .flat_map(|x| {
                let heights = &heights;
                (*y0..=*y1).map(move |y| heights.get(&(x, y)).copied().unwrap_or(0))
            })
            .max()?;
        (*z0, *z1) = (z + 1, z + 1 - *z0 + *z1);
        let z = *z1;
        heights.extend((*x0..=*x1).flat_map(|x| (*y0..=*y1).map(move |y| ((x, y), z))));
    }
    bricks.sort_by_key(|((_, _, z), _)| *z);
    let (mut rdeps, mut deps) = (Multimap::new(), Multimap::new());
    for (i, ((x0, y0, _), (x1, y1, z1))) in bricks.iter().enumerate() {
        for (j, ((x2, y2, _), (x3, y3, _))) in bricks
            .iter()
            .enumerate()
            .skip(i + 1)
            .skip_while(|(_, ((_, _, z2), _))| *z2 <= *z1)
            .take_while(|(_, ((_, _, z2), _))| *z2 == *z1 + 1)
        {
            if *x0 <= *x3 && *x2 <= *x1 && *y0 <= *y3 && *y2 <= *y1 {
                rdeps.entry(i).or_insert_with(BTreeSet::new).insert(j);
                deps.entry(j).or_insert_with(BTreeSet::new).insert(i);
            }
        }
    }
    Some((bricks.len(), rdeps, deps))
}

pub fn part1(data: &str) -> Option<usize> {
    let (size, _, deps) = parse(data)?;
    Some(
        size - deps
            .values()
            .filter_map(|values| {
                let mut iter = values.iter();
                iter.next().filter(|_| iter.next().is_none())
            })
            .collect::<BTreeSet<_>>()
            .len(),
    )
}

pub fn part2(data: &str) -> Option<usize> {
    let (_, rdeps, deps) = parse(data)?;
    Some(
        rdeps
            .par_iter()
            .map(|item| {
                let mut sum = 0usize;
                let mut deps = deps.clone();
                let mut stack = vec![item];
                while let Some((below, aboves)) = stack.pop() {
                    for above in aboves {
                        let Entry::Occupied(mut entry) = deps.entry(*above) else {
                            continue;
                        };
                        if entry.get_mut().remove(below) && entry.get().is_empty() {
                            sum += 1;
                            entry.remove();
                            if let Some(above2) = rdeps.get(above) {
                                stack.push((above, above2));
                            }
                        }
                    }
                }
                sum
            })
            .sum(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        1,0,1~1,2,1
        0,0,2~2,0,2
        0,2,3~2,2,3
        0,0,4~0,2,4
        2,0,5~2,2,5
        0,1,6~2,1,6
        1,1,8~1,1,9
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(5), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(7), part2(EXAMPLE));
    }
}
