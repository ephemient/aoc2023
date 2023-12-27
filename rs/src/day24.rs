use num_rational::Ratio;
use num_traits::cast::ToPrimitive;
use std::cmp::Ordering;
use std::error;
use std::fmt::{self, Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::ops::RangeInclusive;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum Error<FormatErr> {
    Format(FormatErr),
    Parse,
    Solution,
}

impl<FormatError> From<FormatError> for Error<FormatError> {
    fn from(value: FormatError) -> Self {
        Self::Format(value)
    }
}

impl<FormatError: Display> Display for Error<FormatError> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::Format(err) => write!(f, "Format({})", err),
            Error::Parse => write!(f, "Parse"),
            Error::Solution => write!(f, "Solution"),
        }
    }
}

impl<FormatError: error::Error> error::Error for Error<FormatError> {}

fn parse_helper<T: FromStr>(s: &str) -> Result<(T, T, T), Error<T::Err>> {
    let mut iter = s.splitn(3, ',');
    Ok((
        iter.next().ok_or(Error::Parse)?.trim().parse()?,
        iter.next().ok_or(Error::Parse)?.trim().parse()?,
        iter.next().ok_or(Error::Parse)?.trim().parse()?,
    ))
}

#[allow(clippy::type_complexity)]
fn parse<T: FromStr>(data: &str) -> Result<Vec<((T, T, T), (T, T, T))>, Error<T::Err>> {
    data.lines()
        .map(|line| {
            let Some((pos, vel)) = line.split_once('@') else {
                return Err(Error::Parse);
            };
            Ok((parse_helper(pos)?, parse_helper(vel)?))
        })
        .collect()
}

fn part1_internal(data: &str, lo: f64, hi: f64) -> Result<usize, Error<ParseFloatError>> {
    let mut lines = Vec::<(f64, f64, RangeInclusive<f64>)>::new();
    parse::<f64>(data)?
        .into_iter()
        .map(|((x0, y0, _), (vx0, vy0, _))| {
            let m0 = vy0 / vx0;
            let b0 = y0 - m0 * x0;
            let range0 = match vx0.partial_cmp(&0.0)? {
                Ordering::Less => -f64::INFINITY..=x0,
                Ordering::Equal => return None,
                Ordering::Greater => x0..=f64::INFINITY,
            };
            let n = lines
                .iter()
                .filter(|(m1, b1, range1)| {
                    let x = (b0 - b1) / (m1 - m0);
                    m0 != *m1
                        && (lo..=hi).contains(&x)
                        && (lo..=hi).contains(&(m0 * x + b0))
                        && range0.contains(&x)
                        && range1.contains(&x)
                })
                .count();
            lines.push((m0, b0, range0));
            Some(n)
        })
        .try_fold(0, |acc, n| Some(acc + n?))
        .ok_or(Error::Solution)
}

pub fn part1(data: &str) -> Result<usize, Error<ParseFloatError>> {
    part1_internal(data, 2e14, 4e14)
}

pub fn part2(data: &str) -> Result<i64, Error<ParseIntError>> {
    let data = parse::<i64>(data)?;
    for (i, (pos0, vel0)) in data.iter().enumerate() {
        let pos0 = (i128::from(pos0.0), i128::from(pos0.1), i128::from(pos0.2));
        let vel0 = (i128::from(vel0.0), i128::from(vel0.1), i128::from(vel0.2));

        for (j, (pos1, vel1)) in data.iter().enumerate().skip(i + 1) {
            let pos1 = (
                i128::from(pos1.0) - pos0.0,
                i128::from(pos1.1) - pos0.1,
                i128::from(pos1.2) - pos0.2,
            );
            let vel1 = (
                i128::from(vel1.0) - vel0.0,
                i128::from(vel1.1) - vel0.1,
                i128::from(vel1.2) - vel0.2,
            );
            let plane1 = (
                pos1.1 * vel1.2 - pos1.2 * vel1.1,
                pos1.2 * vel1.0 - pos1.0 * vel1.2,
                pos1.0 * vel1.1 - pos1.1 * vel1.0,
            );

            for (pos2, vel2) in data.iter().skip(j + 1) {
                let pos2 = (
                    i128::from(pos2.0) - pos0.0,
                    i128::from(pos2.1) - pos0.1,
                    i128::from(pos2.2) - pos0.2,
                );
                let vel2 = (
                    i128::from(vel2.0) - vel0.0,
                    i128::from(vel2.1) - vel0.1,
                    i128::from(vel2.2) - vel0.2,
                );
                let plane2 = (
                    pos2.1 * vel2.2 - pos2.2 * vel2.1,
                    pos2.2 * vel2.0 - pos2.0 * vel2.2,
                    pos2.0 * vel2.1 - pos2.1 * vel2.0,
                );

                let line = (
                    plane1.1 * plane2.2 - plane1.2 * plane2.1,
                    plane1.2 * plane2.0 - plane1.0 * plane2.2,
                    plane1.0 * plane2.1 - plane1.1 * plane2.0,
                );
                if line.1 * vel1.0 == line.0 * vel1.1 || line.1 * vel2.0 == line.0 * vel2.1 {
                    continue;
                }

                let u1 = Ratio::new(
                    pos1.1 * vel1.0 - pos1.0 * vel1.1,
                    line.1 * vel1.0 - line.0 * vel1.1,
                );
                let u2 = Ratio::new(
                    pos2.1 * vel2.0 - pos2.0 * vel2.1,
                    line.1 * vel2.0 - line.0 * vel2.1,
                );
                let t1 = if vel1.0 != 0 {
                    (Ratio::from(line.0) * u1 - Ratio::from(pos1.0)) / Ratio::from(vel1.0)
                } else if vel1.1 != 0 {
                    (Ratio::from(line.1) * u1 - Ratio::from(pos1.1)) / Ratio::from(vel1.1)
                } else if vel1.2 != 0 {
                    (Ratio::from(line.2) * u1 - Ratio::from(pos1.2)) / Ratio::from(vel1.2)
                } else {
                    continue;
                };
                let t2 = if vel2.0 != 0 {
                    (Ratio::from(line.0) * u2 - Ratio::from(pos2.0)) / Ratio::from(vel2.0)
                } else if vel2.1 != 1 {
                    (Ratio::from(line.1) * u2 - Ratio::from(pos2.1)) / Ratio::from(vel2.1)
                } else if vel2.2 != 1 {
                    (Ratio::from(line.2) * u2 - Ratio::from(pos2.2)) / Ratio::from(vel2.2)
                } else {
                    continue;
                };
                if t1 == t2 {
                    continue;
                }

                return (Ratio::from(pos0.0 + pos0.1 + pos0.2)
                    + Ratio::from(line.0 + line.1 + line.2) * (u1 * t2 - u2 * t1) / (t2 - t1))
                    .to_i64()
                    .ok_or(Error::Solution);
            }
        }
    }
    Err(Error::Solution)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        19, 13, 30 @ -2,  1, -2
        18, 19, 22 @ -1, -1, -2
        20, 25, 34 @ -2, -2, -4
        12, 31, 28 @ -1, -2, -1
        20, 19, 15 @  1, -5, -3
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Ok(2), part1_internal(EXAMPLE, 7.0, 27.0));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Ok(47), part2(EXAMPLE));
    }
}
