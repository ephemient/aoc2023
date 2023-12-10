use std::cmp::Reverse;
use std::collections::{BTreeSet, BinaryHeap};

enum Direction {
    U,
    L,
    D,
    R,
}

fn part1_helper(maze: &[&str]) -> Option<(usize, BTreeSet<(usize, usize)>)> {
    let mut queue = BinaryHeap::new();
    for (y, line) in maze.iter().enumerate() {
        for (x, c) in line.char_indices() {
            if c == 'S' {
                queue.push((Reverse(0), (y, x)));
            }
        }
    }
    let mut last_d = None;
    let mut visited = BTreeSet::new();
    while let Some((Reverse(d), (y, x))) = queue.pop() {
        let directions = match maze[y..]
            .iter()
            .next()
            .and_then(|line| line[x..].chars().next())
        {
            Some('S') => vec![Direction::U, Direction::L, Direction::D, Direction::R],
            Some('|') => vec![Direction::U, Direction::D],
            Some('-') => vec![Direction::L, Direction::R],
            Some('L') => vec![Direction::U, Direction::R],
            Some('J') => vec![Direction::U, Direction::L],
            Some('7') => vec![Direction::L, Direction::D],
            Some('F') => vec![Direction::D, Direction::R],
            _ => continue,
        };
        if !visited.insert((y, x)) {
            continue;
        }
        last_d = Some(d);
        for direction in directions {
            if let Some(next) = match direction {
                Direction::U => y.checked_sub(1).map(|y| (y, x)),
                Direction::L => x.checked_sub(1).map(|x| (y, x)),
                Direction::D => Some((y + 1, x)),
                Direction::R => Some((y, x + 1)),
            } {
                queue.push((Reverse(d + 1), next));
            }
        }
    }
    Some((last_d?, visited))
}

pub fn part1(data: &str) -> Option<usize> {
    Some(part1_helper(&data.lines().collect::<Vec<_>>())?.0)
}

pub fn part2(data: &str) -> Option<usize> {
    let maze = data.lines().collect::<Vec<_>>();
    let width = maze.iter().map(|line| line.len()).max().unwrap_or(0);
    let height = maze.len();
    let mask = part1_helper(&maze)?.1;
    let lookup = |(y, x)| {
        if mask.contains(&(y, x)) {
            maze[y..]
                .iter()
                .next()
                .and_then(|line| line[x..].chars().next())
        } else {
            None
        }
    };
    let mut stack = (0..=width)
        .map(|x| (0, x))
        .chain((0..=height).map(|y| (y, 0)))
        .chain((0..=width).map(|x| (height, x)))
        .chain((0..=height).map(|y| (y, width)))
        .collect::<Vec<_>>();
    let mut visited = BTreeSet::new();
    while let Some((y, x)) = stack.pop() {
        if !visited.insert((y, x)) {
            continue;
        }
        let up_left = y.checked_sub(1).zip(x.checked_sub(1)).and_then(lookup);
        let up_right = y.checked_sub(1).and_then(|y| lookup((y, x)));
        let down_left = x.checked_sub(1).and_then(|x| lookup((y, x)));
        let down_right = lookup((y, x));
        if y > 0 {
            match up_left {
                Some('|') | Some('J') | Some('7') | None => match up_right {
                    Some('|') | Some('L') | Some('F') | None => stack.push((y - 1, x)),
                    _ => {}
                },
                _ => {}
            };
        }
        if x > 0 {
            match up_left {
                Some('-') | Some('L') | Some('J') | None => match down_left {
                    Some('-') | Some('7') | Some('F') | None => stack.push((y, x - 1)),
                    _ => {}
                },
                _ => {}
            };
        }
        if y < height {
            match down_left {
                Some('|') | Some('J') | Some('7') | None => match down_right {
                    Some('|') | Some('L') | Some('F') | None => stack.push((y + 1, x)),
                    _ => {}
                },
                _ => {}
            };
        }
        if x < width {
            match up_right {
                Some('-') | Some('L') | Some('J') | None => match down_right {
                    Some('-') | Some('7') | Some('F') | None => stack.push((y, x + 1)),
                    _ => {}
                },
                _ => {}
            };
        }
    }
    let visited = visited
        .iter()
        .copied()
        .filter(|&(y, x)| visited.contains(&(y + 1, x)))
        .collect::<BTreeSet<_>>();
    let visited = visited
        .iter()
        .copied()
        .filter(|&(y, x)| visited.contains(&(y, x + 1)))
        .collect::<BTreeSet<_>>();
    Some(width * height - mask.len() - visited.len())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        -L|F7
        7S-7|
        L|7||
        -L-J|
        L|-JF
    "};
    static EXAMPLE_2: &str = indoc! {"
        7-F7-
        .FJ|7
        SJLL7
        |F--J
        LJ.LJ
    "};
    static EXAMPLE_3: &str = indoc! {"
        ...........
        .S-------7.
        .|F-----7|.
        .||.....||.
        .||.....||.
        .|L-7.F-J|.
        .|..|.|..|.
        .L--J.L--J.
        ...........
    "};
    static EXAMPLE_4: &str = indoc! {"
        ..........
        .S------7.
        .|F----7|.
        .||....||.
        .||....||.
        .|L-7F-J|.
        .|..||..|.
        .L--JL--J.
        ..........
    "};
    static EXAMPLE_5: &str = indoc! {"
        .F----7F7F7F7F-7....
        .|F--7||||||||FJ....
        .||.FJ||||||||L7....
        FJL7L7LJLJ||LJ.L-7..
        L--J.L7...LJS7F-7L7.
        ....F-J..F7FJ|L7L7L7
        ....L7.F7||L7|.L7L7|
        .....|FJLJ|FJ|F7|.LJ
        ....FJL-7.||.||||...
        ....L---J.LJ.LJLJ...
    "};
    static EXAMPLE_6: &str = indoc! {"
        FF7FSF7F7F7F7F7F---7
        L|LJ||||||||||||F--J
        FL-7LJLJ||||||LJL-77
        F--JF--7||LJLJ7F7FJ-
        L---JF-JLJ.||-FJLJJ7
        |F|F-JF---7F7-L7L|7|
        |FFJF7L7F-JF7|JL---7
        7-L-JL7||F7|L7F-7F7|
        L.L7LFJ|||||FJL7||LJ
        L7JLJL-JLJLJL--JLJ.L
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(4), part1(EXAMPLE_1));
        assert_eq!(Some(8), part1(EXAMPLE_2));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(4), part2(EXAMPLE_3));
        assert_eq!(Some(4), part2(EXAMPLE_4));
        assert_eq!(Some(8), part2(EXAMPLE_5));
        assert_eq!(Some(10), part2(EXAMPLE_6));
    }
}
