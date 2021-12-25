use aoc21::read_file;
use std::error::Error;

fn part1(input: &[i32]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .windows(2)
        .map(|a| if a[0] < a[1] { 1 } else { 0 })
        .sum())
}

fn part2(input: &[i32]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .windows(4)
        .map(|i| {
            if i[0] + i[1] + i[2] < i[1] + i[2] + i[3] {
                1
            } else {
                0
            }
        })
        .sum())
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents: Vec<_> = read_file()?
        .iter()
        .map(|s| s.parse::<i32>().unwrap())
        .collect();
    println!("Prob1: {}", part1(&contents)?);
    println!("Prob2: {}", part2(&contents)?);
    Ok(())
}
