use aoc2023::day1;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
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
    Ok(())
}

criterion_group!(aoc2023, aoc2023_bench);
criterion_main!(aoc2023);
