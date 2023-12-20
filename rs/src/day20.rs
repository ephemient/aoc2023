use num_integer::lcm;
use std::collections::{HashMap, HashSet, VecDeque};

type Machines<'a> = HashMap<&'a str, (Option<Type>, HashSet<&'a str>)>;
type Dependencies<'a> = HashMap<&'a str, HashSet<&'a str>>;

#[derive(Clone, Copy, Debug)]
enum Type {
    FlipFlop,
    Conjunction,
}

#[derive(Clone, Debug)]
enum State<'a> {
    FlipFlop(bool),
    Conjunction(HashMap<&'a str, bool>),
}

impl<'a> State<'a> {
    fn new(key: &'a str, type_: Type, dependencies: &Dependencies<'a>) -> State<'a> {
        match type_ {
            Type::FlipFlop => Self::FlipFlop(false),
            Type::Conjunction => Self::Conjunction(dependencies.get(&key).map_or_else(
                || [].into(),
                |vec| vec.iter().map(|&key| (key, false)).collect(),
            )),
        }
    }

    fn pulse(&mut self, key: &'a str, value: bool) -> Option<bool> {
        match self {
            State::FlipFlop(flip_flop) => {
                if value {
                    None
                } else {
                    *flip_flop = !*flip_flop;
                    Some(*flip_flop)
                }
            }
            State::Conjunction(remembered) => {
                *remembered.get_mut(key)? = value;
                Some(!remembered.iter().all(|(_, &value)| value))
            }
        }
    }

    fn value(&self) -> bool {
        match self {
            State::FlipFlop(value) => *value,
            State::Conjunction(remembered) => !remembered.iter().all(|(_, &value)| value),
        }
    }
}

fn parse(data: &str) -> Option<(Machines, Dependencies)> {
    let mut machines = HashMap::new();
    let mut dependencies = HashMap::new();
    for line in data.lines() {
        let Some((lhs, rhs)) = line.split_once(" -> ") else {
            continue;
        };
        let (key, type_) = if let Some(key) = lhs.strip_prefix('%') {
            (key, Some(Type::FlipFlop))
        } else if let Some(key) = lhs.strip_prefix('&') {
            (key, Some(Type::Conjunction))
        } else {
            (lhs, None)
        };
        let rhs = rhs.split(", ").collect();
        for &dst in &rhs {
            dependencies
                .entry(dst)
                .or_insert_with(HashSet::new)
                .insert(key);
        }
        machines.insert(key, (type_, rhs));
    }
    Some((machines, dependencies))
}

pub fn part1(data: &str) -> Option<u32> {
    let (machines, dependencies) = parse(data)?;
    let mut state = machines
        .iter()
        .filter_map(|(&key, (type_, _))| Some((key, State::new(key, (*type_)?, &dependencies))))
        .collect::<HashMap<_, _>>();
    let (mut x, mut y) = (0, 0);
    for _ in 0..1000 {
        let mut queue: VecDeque<_> = [("button", "broadcaster", false)].into();
        while let Some((src, key, value)) = queue.pop_front() {
            if value {
                x += 1;
            } else {
                y += 1
            }
            let Some(value) = (match state.get_mut(key) {
                Some(state) => state.pulse(src, value),
                None => Some(value),
            }) else {
                continue;
            };
            let Some((_, dsts)) = machines.get(key) else {
                continue;
            };
            queue.extend(dsts.iter().map(|&dst| (key, dst, value)));
        }
    }
    Some(x * y)
}

pub fn part2(data: &str) -> Option<usize> {
    let (machines, dependencies) = parse(data)?;
    let subsets = dependencies
        .get({
            let mut iter = dependencies.get("rx")?.iter();
            let &conjunction = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            conjunction
        })?
        .iter()
        .map(|&dst| {
            let mut seen = HashSet::new();
            let mut stack = vec![dst];
            while let Some(key) = stack.pop() {
                if !seen.insert(key) {
                    continue;
                }
                if let Some(keys) = dependencies.get(key) {
                    stack.extend(keys);
                };
            }
            (dst, seen)
        })
        .collect::<Vec<_>>();
    for (i, (_, s0)) in subsets.iter().enumerate() {
        for (_, s1) in &subsets[i + 1..] {
            let mut iter = s0.intersection(s1);
            if !iter.next()?.eq(&"broadcaster") || iter.next().is_some() {
                return None;
            }
        }
    }
    subsets.into_iter().try_fold(1, |acc, (_, subset)| {
        let mut state = machines
            .iter()
            .filter_map(|(&key, (type_, _))| {
                if subset.contains(key) {
                    Some((key, State::new(key, (*type_)?, &dependencies)))
                } else {
                    None
                }
            })
            .collect::<HashMap<_, _>>();
        let mut seen = HashMap::new();
        let size = loop {
            let mut snapshot = state
                .iter()
                .map(|(&key, value)| (key, value.value()))
                .collect::<Vec<_>>();
            snapshot.sort_by(|(a, _), (b, _)| a.cmp(b));
            let i = seen.len();
            match seen.entry(snapshot) {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    break Some(i - *entry.get());
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(i);
                }
            }
            let mut queue: VecDeque<_> = [("button", "broadcaster", false)].into();
            while let Some((src, key, value)) = queue.pop_front() {
                let Some(value) = (match state.get_mut(key) {
                    Some(state) => state.pulse(src, value),
                    None => Some(value),
                }) else {
                    continue;
                };
                let Some((_, dsts)) = machines.get(key) else {
                    continue;
                };
                queue.extend(dsts.intersection(&subset).map(|&dst| (key, dst, value)));
            }
        }?;
        Some(lcm(acc, size))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        broadcaster -> a, b, c
        %a -> b
        %b -> c
        %c -> inv
        &inv -> a
    "};
    static EXAMPLE_2: &str = indoc! {"
        broadcaster -> a
        %a -> inv, con
        &inv -> b
        %b -> con
        &con -> output
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(32000000), part1(EXAMPLE_1));
        assert_eq!(Some(11687500), part1(EXAMPLE_2));
    }

    #[test]
    #[ignore]
    fn part2_examples() {}
}
