use aoc2023::{day1, day2, day3};
use criterion::{black_box, Criterion};
use std::env;
use std::fs;
use std::io;
use std::path::Path;

fn get_day_input(day: u8) -> Result<String, io::Error> {
    let datadir = env::var("AOC2023_DATADIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| ".".to_string());
    fs::read_to_string(Path::new(&datadir).join(format!("day{}.txt", day)))
}

fn aoc2023_bench(c: &mut Criterion) -> Result<(), io::Error> {
    let data = get_day_input(1)?;
    let mut g = c.benchmark_group("day 1");
    g.bench_function("part 1", |b| b.iter(|| day1::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day1::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(2)?;
    let mut g = c.benchmark_group("day 2");
    g.bench_function("part 1", |b| b.iter(|| day2::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day2::part2(black_box(&data))));
    g.finish();

    let data = get_day_input(3)?;
    let mut g = c.benchmark_group("day 3");
    g.bench_function("part 1", |b| b.iter(|| day3::part1(black_box(&data))));
    g.bench_function("part 2", |b| b.iter(|| day3::part2(black_box(&data))));
    g.finish();

    Ok(())
}

fn aoc2023() -> Result<(), io::Error> {
    let mut criterion = Criterion::default().configure_from_args();
    aoc2023_bench(&mut criterion)?;
    Ok(())
}

fn main() -> Result<(), io::Error> {
    aoc2023()?;
    Criterion::default().configure_from_args().final_summary();
    Ok(())
}
