use std::cmp::max;
use std::collections::BTreeMap;

fn parse(data: &str) -> Vec<(u32, BTreeMap<&str, u32>)> {
    data.lines()
        .filter_map(|line| {
            let line = line.strip_prefix("Game ")?;
            let (game_id, line) = line.split_once(':')?;
            let game_id = game_id.parse().ok()?;
            let mut cubes = BTreeMap::<&str, u32>::new();
            for part in line.split([',', ';']) {
                let (count, color) = part.strip_prefix(' ')?.split_once(' ')?;
                let count = count.parse().ok()?;
                cubes
                    .entry(color)
                    .and_modify(|x| *x = max(*x, count))
                    .or_insert(count);
            }
            Some((game_id, cubes))
        })
        .collect()
}

pub fn part1(data: &str) -> u32 {
    parse(data)
        .into_iter()
        .filter_map(|(game_id, mut cubes)| {
            if cubes.remove("red").unwrap_or(0) <= 12
                && cubes.remove("green").unwrap_or(0) <= 13
                && cubes.remove("blue").unwrap_or(0) <= 14
                && cubes.is_empty()
            {
                Some(game_id)
            } else {
                None
            }
        })
        .sum()
}

pub fn part2(data: &str) -> u32 {
    parse(data)
        .into_iter()
        .map(|(_, cubes)| cubes.into_values().product::<u32>())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(8, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(2286, part2(EXAMPLE));
    }
}
