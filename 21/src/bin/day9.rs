use aoc21::read_file;
use std::{collections::HashSet, error::Error};

fn part1(input: &[Vec<u8>]) -> Result<i32, Box<dyn Error>> {
    let mut risk_lvl: i32 = 0;
    let offsets = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    for i in 0..input.len() {
        for j in 0..input[0].len() {
            let v = input[i][j];
            let mut is_min = true;
            for offset in offsets {
                if (i == 0 && offset[0] == -1)
                    || (j == 0 && offset[1] == -1)
                    || i as i32 + offset[0] == input.len() as i32
                    || j as i32 + offset[1] == input[0].len() as i32
                {
                    continue;
                }
                if input[(i as i32 + offset[0]) as usize][(j as i32 + offset[1]) as usize] <= v {
                    is_min = false;
                    break;
                }
            }
            if is_min {
                risk_lvl += input[i][j] as i32 + 1;
            }
        }
    }
    Ok(risk_lvl)
}

fn get_neighbor_idxs(i: usize, j: usize, max_i: usize, max_j: usize) -> Vec<(usize, usize)> {
    let mut rv = Vec::new();
    if i + 2 < max_i {
        rv.push((i + 1, j))
    }
    if i > 0 {
        rv.push((i - 1, j))
    }
    if j + 2 < max_j {
        rv.push((i, j + 1))
    }
    if j > 0 {
        rv.push((i, j - 1))
    }
    rv
}

fn part2(input: &[Vec<u8>]) -> Result<i32, Box<dyn Error>> {
    let mut minima_size = Vec::new();
    let offsets = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    for i in 0..input.len() {
        for j in 0..input[0].len() {
            let v = input[i][j];
            let mut is_min = true;
            for offset in offsets {
                if (i == 0 && offset[0] == -1)
                    || (j == 0 && offset[1] == -1)
                    || i as i32 + offset[0] == input.len() as i32
                    || j as i32 + offset[1] == input[0].len() as i32
                {
                    continue;
                }
                if input[(i as i32 + offset[0]) as usize][(j as i32 + offset[1]) as usize] <= v {
                    is_min = false;
                    break;
                }
            }
            if is_min {
                let mut tovisit = Vec::new();
                let mut visited = HashSet::new();
                tovisit.push((i, j));
                let mut sz = 0;
                while let Some(cur) = tovisit.pop() {
                    sz += 1;
                    visited.insert(cur);
                    for neighbor in get_neighbor_idxs(cur.0, cur.1, input.len(), input[0].len()) {
                        if !visited.contains(&neighbor)
                            && input[cur.0][cur.1] < input[neighbor.0][neighbor.1]
                        {
                            tovisit.push(neighbor);
                        }
                    }
                }
                minima_size.push(sz);
            }
        }
    }
    minima_size.sort();
    Ok(minima_size.iter().rev().take(3).fold(1, |acc, el| acc * el))
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let input: Vec<Vec<u8>> = read_file()?
        .iter()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect()
        })
        .collect();
    println!("{}", part1(&*input)?);
    println!("{}", part2(&input)?);
    Ok(())
}
