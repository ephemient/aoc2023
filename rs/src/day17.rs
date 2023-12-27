use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Direction {
    U,
    L,
    D,
    R,
}

impl Direction {
    fn next(&self) -> Direction {
        match self {
            Direction::U => Direction::L,
            Direction::L => Direction::D,
            Direction::D => Direction::R,
            Direction::R => Direction::U,
        }
    }
    fn prev(&self) -> Direction {
        match self {
            Direction::U => Direction::R,
            Direction::L => Direction::U,
            Direction::D => Direction::L,
            Direction::R => Direction::D,
        }
    }
}

fn step(y: usize, x: usize, dir: Direction) -> Option<(usize, usize)> {
    Some(match dir {
        Direction::U => (y.checked_sub(1)?, x),
        Direction::L => (y, x.checked_sub(1)?),
        Direction::D => (y.checked_add(1)?, x),
        Direction::R => (y, x.checked_add(1)?),
    })
}

fn solve<P, F, T>(data: &str, ok: P, next: F) -> Option<usize>
where
    P: Fn(usize) -> bool,
    F: Fn(Direction, usize) -> T,
    T: IntoIterator,
    T::Item: Into<Direction>,
{
    let maze = data
        .lines()
        .filter_map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).filter(|&d| d > 0))
                .collect::<Option<Vec<_>>>()
        })
        .collect::<Vec<_>>();
    let mut queue: BinaryHeap<_> = [(Reverse(0), 0, 0, Direction::R, 0)].into();
    let mut costs: HashMap<_, _> = [((0, 0, Direction::R, 0), 0)].into();
    while let Some((Reverse(cost), y, x, direction @ direction0, distance)) = queue.pop() {
        let cost = cost + y + x;
        if y == maze.len() - 1 && x == maze[y].len() - 1 && ok(distance) {
            return Some(cost);
        }
        match costs.entry((y, x, direction, distance)) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                if *entry.get() < cost {
                    continue;
                }
            }
            std::collections::hash_map::Entry::Vacant(_) => {
                #[cfg(debug_assertions)]
                panic!(
                    "missing state for ({}, {}, {}, {:?}, {})",
                    cost, y, x, direction, distance
                );
            }
        }
        for direction in next(direction, distance) {
            let direction = direction.into();
            let Some((y, x)) =
                step(y, x, direction).filter(|&(y, x)| y < maze.len() && x < maze[y].len())
            else {
                continue;
            };
            let distance = if direction == direction0 {
                distance + 1
            } else {
                1
            };
            let cost = cost + maze[y][x] as usize;
            match costs.entry((y, x, direction, distance)) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    if *entry.get() <= cost {
                        continue;
                    }
                    entry.insert(cost);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(cost);
                }
            }
            queue.push((Reverse(cost - y - x), y, x, direction, distance));
        }
    }
    None
}

pub fn part1(data: &str) -> Option<usize> {
    solve(
        data,
        |_| true,
        |direction, distance| {
            if distance < 3 {
                vec![direction.prev(), direction.next(), direction]
            } else {
                vec![direction.prev(), direction.next()]
            }
        },
    )
}

pub fn part2(data: &str) -> Option<usize> {
    solve(
        data,
        |distance| distance >= 4,
        |direction, distance| {
            if distance < 4 {
                vec![direction]
            } else if distance < 10 {
                vec![direction.prev(), direction.next(), direction]
            } else {
                vec![direction.prev(), direction.next()]
            }
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        2413432311323
        3215453535623
        3255245654254
        3446585845452
        4546657867536
        1438598798454
        4457876987766
        3637877979653
        4654967986887
        4564679986453
        1224686865563
        2546548887735
        4322674655533
    "};
    static EXAMPLE_2: &str = indoc! {"
        111111111111
        999999999991
        999999999991
        999999999991
        999999999991
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(102), part1(EXAMPLE_1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(94), part2(EXAMPLE_1));
        assert_eq!(Some(71), part2(EXAMPLE_2));
    }
}
