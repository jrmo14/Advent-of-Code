use aoc21::read_file;
use std::error::Error;

type T = i32;

fn part1(input: &[T]) -> Result<i32, Box<dyn Error>> {
    let mut sorted = input.to_owned();
    sorted.sort_unstable();
    let pos = sorted[sorted.len() / 2];
    Ok(sorted.iter().map(|v| i32::abs(pos - v)).sum())
}

fn part2(input: &[T]) -> Result<i32, Box<dyn Error>> {
    let mean = input.iter().sum::<T>() as f64 / input.len() as f64;
    let compute_cost = |v| v * (v + 1.0) / 2.0;
    let floor_cost: i32 = input
        .iter()
        .map(|&v| compute_cost(f64::abs(mean.floor() - v as f64)) as i32)
        .sum();
    let ceil_cost: i32 = input
        .iter()
        .map(|&v| compute_cost(f64::abs(mean.ceil() - v as f64)) as i32)
        .sum();

    Ok(floor_cost.min(ceil_cost))
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let input: Vec<_> = read_file()?[0]
        .split(',')
        .map(|v| v.parse().unwrap())
        .collect();
    println!("{}", part1(&input)?);
    println!("{}", part2(&input)?);

    Ok(())
}
