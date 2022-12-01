use aoc21::read_file;
use std::error::Error;

type T = String;

fn part1(input: &[T]) -> Result<i32, Box<dyn Error>> {
    let gamma_str = input
        .iter()
        .fold(vec![0_i32; input[0].len()], |acc: Vec<i32>, el: &String| {
            acc.iter()
                .zip(el.chars())
                .map(|(&a, c)| match c {
                    '1' => a + 1,
                    '0' => a - 1,
                    _ => a,
                })
                .collect()
        })
        .iter()
        .map(|&v| if v > 0 { '1' } else { '0' })
        .collect::<String>();
    let gamma: i32 = i32::from_str_radix(&gamma_str, 2)?;
    let eps: i32 = (!gamma) & ((1 << (input[0].len() - 1)) - 1);
    Ok(eps * gamma)
}

fn get_remove_idxs(nums: &[T], c: char, idx: usize) -> Vec<usize> {
    nums.iter()
        .enumerate()
        .filter_map(|(i, e)| {
            if e.chars().nth(idx).unwrap() == c {
                None
            } else {
                Some(i)
            }
        })
        .collect()
}

fn count_candidates(candidates: &[T], idx: usize) -> i32 {
    candidates
        .iter()
        .map(|n| {
            if n.chars().nth(idx).unwrap() == '1' {
                1
            } else {
                -1
            }
        })
        .sum()
}

fn part2(input: &[T]) -> Result<i32, Box<dyn Error>> {
    let mut o2_candidates = input.to_owned();
    let mut co2_candidates = input.to_owned();

    for i in 0..input[0].len() {
        let o2_count: i32 = count_candidates(&o2_candidates, i);
        let co2_count: i32 = count_candidates(&co2_candidates, i);

        let o2_remove_idxs: Vec<_> = if o2_count >= 0 {
            get_remove_idxs(&o2_candidates, '1', i)
        } else {
            get_remove_idxs(&o2_candidates, '0', i)
        };

        let co2_remove_idxs: Vec<_> = if co2_count >= 0 {
            get_remove_idxs(&co2_candidates, '0', i)
        } else {
            get_remove_idxs(&co2_candidates, '1', i)
        };

        if o2_candidates.len() > 1 {
            for &idx in o2_remove_idxs.iter().rev() {
                o2_candidates.remove(idx);
            }
        }

        if co2_candidates.len() > 1 {
            for &idx in co2_remove_idxs.iter().rev() {
                co2_candidates.remove(idx);
            }
        }
    }
    let o2 = i32::from_str_radix(&o2_candidates[0], 2)?;
    let co2 = i32::from_str_radix(&co2_candidates[0], 2)?;
    Ok(o2 * co2)
}
fn main() -> Result<(), Box<dyn Error>> {
    let input: Vec<T> = read_file()?;
    println!("Prob1: {}", part1(&input)?);
    println!("Prob2: {}", part2(&input)?);
    Ok(())
}
#[cfg(test)]
mod test {
    use crate::{part1, part2};

    #[test]
    fn part1_test() {
        let input = vec![
            "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000",
            "11001", "00010", "01010",
        ]
        .into_iter()
        .map(str::to_owned)
        .collect();

        let ans = part1(&input);
        assert!(ans.is_ok());
        assert!(ans.unwrap() == 198)
    }

    #[test]
    fn part2_test() {
        let input = vec![
            "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000",
            "11001", "00010", "01010",
        ]
        .into_iter()
        .map(str::to_owned)
        .collect();
        let ans = part2(&input);
        assert!(ans.is_ok());
        assert!(ans.unwrap() == 230);
    }
}
