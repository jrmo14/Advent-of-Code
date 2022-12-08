use aoc22::read_file;
use std::error::Error;

fn part1(input: &[Vec<u32>]) -> Result<usize, Box<dyn Error>> {
    let mut viz_mat = vec![vec![false; input[0].len()]; input.len()];
    input.iter().enumerate().for_each(|(i, line)| {
        line.iter()
            .enumerate()
            .fold(None, |max, (j, tree)| match max {
                Some(max) => {
                    if tree > max {
                        viz_mat[i][j] = true;
                        Some(tree)
                    } else {
                        Some(max)
                    }
                }
                None => {
                    viz_mat[i][j] = true;
                    Some(tree)
                }
            });
        line.iter()
            .enumerate()
            .rev()
            .fold(None, |max, (j, tree)| match max {
                Some(max) => {
                    if tree > max {
                        viz_mat[i][j] = true;
                        Some(tree)
                    } else {
                        Some(max)
                    }
                }
                None => {
                    viz_mat[i][j] = true;
                    Some(tree)
                }
            });
    });
    for j in 0..input[0].len() {
        let mut max = None;
        for i in 0..input.len() {
            let tree = input[i][j];
            max = match max {
                Some(max) => {
                    if tree > max {
                        viz_mat[i][j] = true;
                        Some(tree)
                    } else {
                        Some(max)
                    }
                }
                None => {
                    viz_mat[i][j] = true;
                    Some(tree)
                }
            }
        }
        let mut max = None;
        for i in (0..input.len()).rev() {
            let tree = input[i][j];
            max = match max {
                Some(max) => {
                    if tree > max {
                        viz_mat[i][j] = true;
                        Some(tree)
                    } else {
                        Some(max)
                    }
                }
                None => {
                    viz_mat[i][j] = true;
                    Some(tree)
                }
            }
        }
    }

    Ok(viz_mat
        .iter()
        .map(|row| row.iter().map(|&viz| usize::from(viz)).sum::<usize>())
        .sum::<usize>())
}
fn part2(input: &[Vec<u32>]) -> Result<u32, Box<dyn Error>> {
    Ok(input
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .enumerate()
                .map(|(j, &tree)| {
                    (0..4)
                        .map(|dir| {
                            let (di, dj) = match dir {
                                0 => (1, 0),
                                1 => (-1, 0),
                                2 => (0, 1),
                                3 => (0, -1),
                                _ => panic!("uh-oh"),
                            };
                            let mut ni = i as i32;
                            let mut nj = j as i32;
                            let mut dist = 0;
                            while input[ni as usize][nj as usize] < tree
                                || (ni == i as i32 && nj == j as i32)
                            {
                                ni += di;
                                nj += dj;
                                if ni < 0
                                    || ni >= input.len() as i32
                                    || nj < 0
                                    || nj >= input[0].len() as i32
                                {
                                    break;
                                }
                                dist += 1;
                            }
                            dist
                        })
                        .product()
                })
                .max()
                .unwrap()
        })
        .max()
        .unwrap())
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day8")?;
    let input: Vec<_> = binding
        .iter()
        .map(|ln| {
            ln.chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<u32>>()
        })
        .collect();
    println!("Part1: {}", part1(&input)?);
    println!("Part2: {}", part2(&input)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{part1, part2};

    const TEST_DATA: &str = "30373
25512
65332
33549
35390";

    const TEST_DATA2: &str = "11111
10001
10801
10901
10001
10001
11111";

    fn parse(data: &str) -> Vec<Vec<u32>> {
        data.lines()
            .map(|ln| {
                ln.chars()
                    .map(|c| c.to_digit(10).unwrap())
                    .collect::<Vec<u32>>()
            })
            .collect::<Vec<_>>()
    }

    #[test]
    pub fn test1() {
        let input: Vec<_> = parse(TEST_DATA);

        assert_eq!(21, part1(&input).unwrap());
        let input: Vec<_> = parse(TEST_DATA2);

        assert_eq!(22, part1(&input).unwrap())
    }

    #[test]
    pub fn test2() {
        let input = parse(TEST_DATA);
        assert_eq!(8, part2(&input).unwrap());
    }
}
