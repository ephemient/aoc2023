use std::cmp::{max, min, Ordering};
use std::collections::HashMap;

type Comparison = (char, Ordering, u32);
type Rule<'a> = Vec<(&'a str, Option<Comparison>)>;

#[derive(Clone, Debug)]
struct Point<T> {
    x: T,
    m: T,
    a: T,
    s: T,
}

impl<T> Point<T> {
    fn get(&self, key: char) -> Option<&T> {
        match key {
            'x' => Some(&self.x),
            'm' => Some(&self.m),
            'a' => Some(&self.a),
            's' => Some(&self.s),
            _ => None,
        }
    }

    fn get_mut(&mut self, key: char) -> Option<&mut T> {
        match key {
            'x' => Some(&mut self.x),
            'm' => Some(&mut self.m),
            'a' => Some(&mut self.a),
            's' => Some(&mut self.s),
            _ => None,
        }
    }
}

fn parse_rule(line: &str) -> Option<(&str, Rule)> {
    let (name, rest) = line.split_once('{')?;
    let rest = rest.strip_suffix('}')?;
    Some((
        name,
        rest.split(',')
            .map(|s| {
                let Some((comparison, name)) = s.split_once(':') else {
                    return Some((s, None));
                };
                let mut chars = comparison.chars();
                let key = chars.next()?;
                let ordering = match chars.next()? {
                    '<' => Ordering::Less,
                    '>' => Ordering::Greater,
                    _ => return None,
                };
                Some((
                    name,
                    Some((key, ordering, chars.as_str().parse::<u32>().ok()?)),
                ))
            })
            .collect::<Option<Vec<_>>>()?,
    ))
}

fn parse_point(line: &str) -> Option<Point<u32>> {
    let line = line.strip_prefix('{')?.strip_suffix('}')?;
    let (mut x, mut m, mut a, mut s) = (None, None, None, None);
    for part in line.split(',') {
        let (option, part) = if let Some(part) = part.strip_prefix("x=") {
            (&mut x, part)
        } else if let Some(part) = part.strip_prefix("m=") {
            (&mut m, part)
        } else if let Some(part) = part.strip_prefix("a=") {
            (&mut a, part)
        } else if let Some(part) = part.strip_prefix("s=") {
            (&mut s, part)
        } else {
            return None;
        };
        if option.replace(part.parse().ok()?).is_some() {
            return None;
        }
    }
    Some(Point {
        x: x?,
        m: m?,
        a: a?,
        s: s?,
    })
}

pub fn part1(data: &str) -> u32 {
    let mut lines = data.lines().skip_while(|line| line.is_empty());
    let rules = lines
        .by_ref()
        .map_while(parse_rule)
        .collect::<HashMap<_, _>>();
    lines
        .filter_map(|line| {
            let point = parse_point(line)?;
            let mut name = "in";
            while let Some(rules) = rules.get(name) {
                name = rules
                    .iter()
                    .find(|(_, comparison)| {
                        comparison.map_or(true, |(key, ordering, expected)| {
                            point
                                .get(key)
                                .map_or(false, |actual| actual.cmp(&expected) == ordering)
                        })
                    })?
                    .0;
            }
            if name == "A" {
                Some(point.x + point.m + point.a + point.s)
            } else {
                None
            }
        })
        .sum()
}

fn part2_helper(rules: &HashMap<&str, Rule>, name: &str, bounds: Point<(u32, u32)>) -> u64 {
    let Point {
        x: (x0, x1),
        m: (m0, m1),
        a: (a0, a1),
        s: (s0, s1),
    } = bounds;
    if x0 > x1 || m0 > m1 || a0 > a1 || s0 > s1 {
        return 0;
    }
    if name == "A" {
        return (x1 - x0 + 1) as u64
            * (m1 - m0 + 1) as u64
            * (a1 - a0 + 1) as u64
            * (s1 - s0 + 1) as u64;
    }
    let Some(rule) = rules.get(name) else {
        return 0;
    };
    rule.iter()
        .scan(
            Some(bounds.clone()),
            |st, &(name, comparison)| -> Option<u64> {
                let Some((key, ordering, value)) = comparison else {
                    return Some(part2_helper(rules, name, st.take()?));
                };
                let mut bounds = st.clone()?;
                let Some(((lo0, hi0), (lo1, hi1))) =
                    bounds.get_mut(key).zip(st.as_mut()?.get_mut(key))
                else {
                    return Some(0);
                };
                match ordering {
                    Ordering::Less => {
                        *hi0 = min(*hi0, value - 1);
                        *lo1 = max(*lo1, value);
                    }
                    Ordering::Greater => {
                        *lo0 = max(*lo0, value + 1);
                        *hi1 = min(*hi1, value);
                    }
                    Ordering::Equal => panic!(),
                }
                if lo1 > hi1 {
                    *st = None;
                }
                Some(part2_helper(rules, name, bounds))
            },
        )
        .sum()
}

pub fn part2(data: &str) -> u64 {
    part2_helper(
        &data
            .lines()
            .skip_while(|line| line.is_empty())
            .map_while(parse_rule)
            .collect::<HashMap<_, _>>(),
        "in",
        Point {
            x: (1, 4000),
            m: (1, 4000),
            a: (1, 4000),
            s: (1, 4000),
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        px{a<2006:qkq,m>2090:A,rfg}
        pv{a>1716:R,A}
        lnx{m>1548:A,A}
        rfg{s<537:gd,x>2440:R,A}
        qs{s>3448:A,lnx}
        qkq{x<1416:A,crn}
        crn{x>2662:A,R}
        in{s<1351:px,qqz}
        qqz{s>2770:qs,m<1801:hdj,R}
        gd{a>3333:R,R}
        hdj{m>838:A,pv}

        {x=787,m=2655,a=1222,s=2876}
        {x=1679,m=44,a=2067,s=496}
        {x=2036,m=264,a=79,s=2244}
        {x=2461,m=1339,a=466,s=291}
        {x=2127,m=1623,a=2188,s=1013}
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(19114, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(167409079868000, part2(EXAMPLE));
    }
}
