use static_init::dynamic;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Direction {
    U,
    L,
    D,
    R,
}

#[dynamic]
static LUT: BTreeMap<(Direction, char), &'static [Direction]> = {
    static DIRECTIONS: [Direction; 4] = [Direction::U, Direction::D, Direction::L, Direction::R];
    [
        ((Direction::U, '/'), &DIRECTIONS[3..4]),
        ((Direction::U, '\\'), &DIRECTIONS[2..3]),
        ((Direction::U, '-'), &DIRECTIONS[2..4]),
        ((Direction::L, '/'), &DIRECTIONS[1..2]),
        ((Direction::L, '\\'), &DIRECTIONS[0..1]),
        ((Direction::L, '|'), &DIRECTIONS[0..2]),
        ((Direction::D, '/'), &DIRECTIONS[2..3]),
        ((Direction::D, '\\'), &DIRECTIONS[3..4]),
        ((Direction::D, '-'), &DIRECTIONS[2..4]),
        ((Direction::R, '/'), &DIRECTIONS[0..1]),
        ((Direction::R, '\\'), &DIRECTIONS[1..2]),
        ((Direction::R, '|'), &DIRECTIONS[0..2]),
    ]
    .into_iter()
    .collect()
};

fn step(y: usize, x: usize, dir: Direction) -> Option<(usize, usize)> {
    Some(match dir {
        Direction::U => (y.checked_sub(1)?, x),
        Direction::L => (y, x.checked_sub(1)?),
        Direction::D => (y.checked_add(1)?, x),
        Direction::R => (y, x.checked_add(1)?),
    })
}

fn fill(data: &[&str], y: usize, x: usize, d: Direction) -> Option<usize> {
    let mut stack = vec![(y, x, d)];
    let mut visited = stack.iter().copied().collect::<BTreeSet<_>>();
    while let Some((y, x, d)) = stack.pop() {
        for &d in *LUT
            .get(&(d, data[y][x..].chars().next()?))
            .unwrap_or(&&[d][..])
        {
            let Some((y, x)) = step(y, x, d) else {
                continue;
            };
            if y < data.len() && x < data[y].len() && visited.insert((y, x, d)) {
                stack.push((y, x, d));
            }
        }
    }
    Some(
        visited
            .into_iter()
            .map(|(y, x, _)| (y, x))
            .collect::<BTreeSet<_>>()
            .len(),
    )
}

pub fn part1(data: &str) -> Option<usize> {
    fill(&data.lines().collect::<Vec<_>>(), 0, 0, Direction::R)
}

pub fn part2(data: &str) -> Option<usize> {
    let data = data.lines().collect::<Vec<_>>();
    (0..data.len())
        .map(|y| (y, 0, Direction::R))
        .chain((0..data.first()?.len()).map(|x| (0, x, Direction::D)))
        .chain(
            (0..data.len()).filter_map(|y| Some((y, data[y].len().checked_sub(1)?, Direction::L))),
        )
        .chain((0..data.last()?.len()).map(|x| (data.len() - 1, x, Direction::U)))
        .filter_map(|(y, x, d)| fill(&data, y, x, d))
        .max()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {r#"
        .|<2<\....
        |v-v\^....
        .v.v.|->>>
        .v.v.v^.|.
        .v.v.v^...
        .v.v.v^..\
        .v.v/2\\..
        <-2-/vv|..
        .|<<<2-|.\
        .v//.|.v..
    "#};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(46), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(51), part2(EXAMPLE));
    }
}
