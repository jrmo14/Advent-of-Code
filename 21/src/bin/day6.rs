use aoc21::read_file;
use std::error::Error;
type T = i32;

fn part1(input: &[T]) -> Result<i32, Box<dyn Error>> {
    let mut local = input.to_owned();
    for _ in 0..80 {
        let mut new_count = 0;
        for e in local.iter_mut() {
            if *e == 0 {
                new_count += 1;
                *e = 6;
            } else {
                *e -= 1;
            }
        }
        local = local.into_iter().chain((0..new_count).map(|_| 8)).collect();
    }
    Ok(local.len() as i32)
}
fn part2(input: &[T]) -> Result<usize, Box<dyn Error>> {
    let mut bins = input.iter().fold(Vec::from([0; 9]), |mut acc, &v| {
        acc[v as usize] += 1;
        acc
    });

    for _ in 0..256 {
        let new = bins[0];
        bins.rotate_left(1);
        bins[6] += new;
        bins[8] = new;
    }

    Ok(bins.iter().sum())
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

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &[&str] = &["3,4,3,1,2"];

    #[test]
    fn test_solve1() {
        let input: Vec<_> = INPUT[0].split(',').map(|v| v.parse().unwrap()).collect();
        assert_eq!(part1(&input).unwrap(), 5934);
    }

    #[test]
    fn test_solve2() {
        let input: Vec<_> = INPUT[0].split(',').map(|v| v.parse().unwrap()).collect();
        assert_eq!(part2(&input).unwrap(), 26984457539);
    }
}
