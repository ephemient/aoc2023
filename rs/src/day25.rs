use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::Hash;

fn cut<T: Copy + Debug + Eq + Hash + Ord>(gr: &HashMap<T, HashSet<T>>, n: usize) -> Option<usize> {
    if n == 0 {
        let mut components = vec![];
        let mut keys = gr.keys().copied().collect::<HashSet<_>>();
        while let Some(start) = keys.iter().next().copied() {
            keys.remove(&start);
            let mut stack = vec![start];
            let mut n = 0;
            while let Some(node) = stack.pop() {
                n += 1;
                let Some(next) = gr.get(&node) else {
                    continue;
                };
                stack.extend(next.iter().copied().filter(|node| keys.remove(node)));
            }
            components.push(n);
        }
        #[cfg(debug_assertions)]
        eprintln!("{:?}", &components);
        return if components.len() > 1 {
            Some(components.into_iter().product())
        } else {
            None
        };
    }

    let mut weights = HashMap::<_, usize>::new();
    for start in gr.keys().copied() {
        let mut queue = VecDeque::from([(start, Vec::new())]);
        let mut visited = HashSet::from([start]);
        while let Some((node, mut path)) = queue.pop_front() {
            path.iter().rev().fold(node, |node1, node2| {
                *weights
                    .entry((node1.min(*node2), node1.max(*node2)))
                    .or_default() += 1;
                *node2
            });
            path.push(node);
            let Some(next) = gr.get(&node) else {
                continue;
            };
            queue.extend(
                next.iter()
                    .copied()
                    .filter(|node| visited.insert(*node))
                    .map(|node| (node, path.clone())),
            );
        }
    }
    let mut weights = weights.into_iter().collect::<Vec<_>>();
    weights.sort_by_key(|(_, n)| *n);

    for ((a, b), _) in weights.into_iter().rev() {
        #[cfg(debug_assertions)]
        eprintln!("({:?}, {:?})", &a, &b);
        let mut gr = gr.clone();
        let r1 = gr.get_mut(&a).is_some_and(|next| next.remove(&b));
        let r2 = gr.get_mut(&b).is_some_and(|next| next.remove(&a));
        debug_assert!(r1 & r2);
        if let Some(r) = cut(&gr, n - 1) {
            return Some(r);
        }
    }

    None
}

pub fn part1(data: &str) -> Option<usize> {
    let mut gr = HashMap::new();
    for line in data.lines() {
        let Some((src, rhs)) = line.split_once(':') else {
            continue;
        };
        for dst in rhs.split_whitespace() {
            gr.entry(src).or_insert_with(HashSet::new).insert(dst);
            gr.entry(dst).or_insert_with(HashSet::new).insert(src);
        }
    }
    cut(&gr, 3)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        jqt: rhn xhk nvd
        rsh: frs pzl lsr
        xhk: hfx
        cmg: qnr nvd lhk bvb
        rhn: xhk bvb hfx
        bvb: xhk hfx
        pzl: lsr hfx nvd
        qnr: nvd
        ntq: jqt hfx bvb xhk
        nvd: lhk
        lsr: lhk
        rzs: qnr cmg lsr rsh
        frs: qnr lhk lsr
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(54), part1(EXAMPLE));
    }
}
