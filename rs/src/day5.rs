use itertools::Itertools;

struct Mapping {
    start: i64,
    end: i64,
    offset: i64,
}

fn parse(data: &str) -> (Vec<i64>, Vec<Vec<Mapping>>) {
    let Some((first, rest)) = data.split_once("\n\n") else {
        return (vec![], vec![]);
    };
    (
        first
            .split(|c: char| !c.is_ascii_digit())
            .filter_map(|s| s.parse::<i64>().ok())
            .collect(),
        rest.split_terminator("\n\n")
            .map(|stanza| {
                stanza
                    .lines()
                    .filter_map(|line| {
                        let mut iter = line.splitn(3, ' ');
                        let dest = iter.next()?.parse::<i64>().ok()?;
                        let source = iter.next()?.parse::<i64>().ok()?;
                        let length = iter.next()?.parse::<i64>().ok()?;
                        Some(Mapping {
                            start: source,
                            end: source + length,
                            offset: dest - source,
                        })
                    })
                    .sorted_by_key(|mapping| mapping.start)
                    .collect()
            })
            .collect(),
    )
}

fn remap(mappings: &[Mapping], range: (i64, i64)) -> Vec<(i64, i64)> {
    let mut ranges = Vec::new();
    let acc = mappings
        .iter()
        .filter(|mapping| mapping.start < range.1 && range.0 < mapping.end)
        .fold(range.0, |acc, mapping| {
            let start = acc.max(mapping.start);
            let end = range.1.min(mapping.end);
            if acc < start {
                ranges.push((acc, start));
            }
            ranges.push((start + mapping.offset, end + mapping.offset));
            end
        });
    if acc < range.1 {
        ranges.push((acc, range.1));
    }
    ranges
}

pub fn part1(data: &str) -> Option<i64> {
    let (seeds, mappings) = parse(data);
    mappings
        .into_iter()
        .fold(
            seeds.into_iter().map(|x| (x, x + 1)).collect::<Vec<_>>(),
            |ranges, mappings| {
                ranges
                    .into_iter()
                    .flat_map(|range| remap(&mappings, range))
                    .collect()
            },
        )
        .into_iter()
        .map(|(x, _)| x)
        .min()
}

pub fn part2(data: &str) -> Option<i64> {
    let (seeds, mappings) = parse(data);
    mappings
        .into_iter()
        .fold(
            seeds
                .into_iter()
                .chunks(2)
                .into_iter()
                .filter_map(|chunk| {
                    let mut iter = chunk.into_iter();
                    let x = iter.next()?;
                    let y = iter.next()?;
                    Some((x, x + y))
                })
                .collect::<Vec<_>>(),
            |ranges, mappings| {
                ranges
                    .into_iter()
                    .flat_map(|range| remap(&mappings, range))
                    .collect()
            },
        )
        .into_iter()
        .map(|(x, _)| x)
        .min()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        seeds: 79 14 55 13

        seed-to-soil map:
        50 98 2
        52 50 48

        soil-to-fertilizer map:
        0 15 37
        37 52 2
        39 0 15

        fertilizer-to-water map:
        49 53 8
        0 11 42
        42 0 7
        57 7 4

        water-to-light map:
        88 18 7
        18 25 70

        light-to-temperature map:
        45 77 23
        81 45 19
        68 64 13

        temperature-to-humidity map:
        0 69 1
        1 0 69

        humidity-to-location map:
        60 56 37
        56 93 4
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(35), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(46), part2(EXAMPLE));
    }
}
