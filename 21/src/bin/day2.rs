use aoc21::read_file;
use std::error::Error;

fn part1(input: &[(i32, i32)]) -> Result<i32, Box<dyn Error>> {
    let rv = input.iter().fold((0, 0), |acc, el| match el.0 {
        0 => (acc.0 + el.1, acc.1),
        1 => (acc.0, acc.1 + el.1),
        _ => (acc.0, acc.1 - el.1),
    });
    Ok(rv.0 * rv.1)
}

fn part2(input: &[(i32, i32)]) -> Result<i32, Box<dyn Error>> {
    let rv = input.iter().fold((0, 0, 0), |acc, el| match el.0 {
        0 => (acc.0 + el.1, acc.1, acc.2 + acc.1 * el.1),
        1 => (acc.0, acc.1 + el.1, acc.2),
        _ => (acc.0, acc.1 - el.1, acc.2),
    });
    Ok(rv.0 * rv.2)
}

fn main() -> Result<(), Box<dyn Error>> {
    let input: Vec<_> = read_file()?
        .iter()
        .map(|l| {
            let line: Vec<_> = l.split(' ').take(2).collect();
            let dir = match line[0].chars().next().unwrap() {
                'f' => 0,
                'd' => 1,
                _ => 2,
            };
            (dir, line[1].parse::<i32>().unwrap())
        })
        .collect();
    println!("Prob1: {}", part1(&input)?);
    println!("Prob2: {}", part2(&input)?);
    Ok(())
}
