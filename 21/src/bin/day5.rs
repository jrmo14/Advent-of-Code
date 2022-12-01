use aoc21::{read_file, ProblemError};
use std::{collections::HashMap, error::Error, fmt, iter::repeat};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    x: i32,
    y: i32,
}

// Pretty print b/c why not
impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

// Now we can get nice clean parsing
impl TryFrom<&str> for Coord {
    type Error = Box<dyn Error>;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut v_iter = value.split(',');
        let x = v_iter.next().ok_or(ProblemError)?.parse()?;
        let y = v_iter.next().ok_or(ProblemError)?.parse()?;
        Ok(Coord { x, y })
    }
}

#[derive(Debug, Clone, Copy)]
struct Line {
    start: Coord,
    end: Coord,
}

// Pretty print b/c why not
impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ->  {}", self.start, self.end)
    }
}

impl TryFrom<&str> for Line {
    type Error = Box<dyn Error>;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut v_iter = value.split(" -> ");
        let start = Coord::try_from(v_iter.next().ok_or(ProblemError)?)?;
        let end = Coord::try_from(v_iter.next().ok_or(ProblemError)?)?;
        Ok(Line { start, end })
    }
}

type T = Line;

impl Line {
    // Return all of the points along the line
    fn along(&self) -> impl Iterator<Item = Coord> + '_ {
        let min_x = i32::min(self.start.x, self.end.x);
        let min_y = i32::min(self.start.y, self.end.y);
        let max_x = i32::max(self.start.x, self.end.x);
        let max_y = i32::max(self.start.y, self.end.y);

        // The chain repeat thing will fill the iterator with enough of the starting values if it's
        // a horizontal/vertical line
        let x_iter: Box<dyn Iterator<Item = i32>> =
            if self.start.x == min_x {
                Box::new((min_x..=max_x).chain(
                    repeat(min_x).take((max_y - min_y).saturating_sub(max_x - min_x) as usize),
                ))
            } else {
                Box::new((min_x..=max_x).rev().chain(
                    repeat(min_x).take((max_y - min_y).saturating_sub(max_x - min_x) as usize),
                ))
            };

        let y_iter: Box<dyn Iterator<Item = i32>> =
            if self.start.y == min_y {
                Box::new((min_y..=max_y).chain(
                    repeat(min_y).take((max_x - min_x).saturating_sub(max_y - min_y) as usize),
                ))
            } else {
                Box::new((min_y..=max_y).rev().chain(
                    repeat(min_y).take((max_x - min_x).saturating_sub(max_y - min_y) as usize),
                ))
            };

        x_iter.zip(y_iter).map(|(x, y)| Coord { x, y })
    }
}

fn part1(input: &[T]) -> Result<i32, Box<dyn Error>> {
    // Remove any diagonals
    Ok(input
        .iter()
        .filter_map(|&l| {
            if l.start.x == l.end.x || l.start.y == l.end.y {
                Some(l)
            } else {
                None
            }
        })
        .fold(HashMap::new(), |mut acc, l| {
            for pt in l.along() {
                let counter = acc.entry(pt).or_insert(0);
                *counter += 1;
            }
            acc
        })
        .values()
        .filter(|&&v| v > 1)
        .count()
        .try_into()
        .unwrap())
}

fn part2(input: &[T]) -> Result<i32, Box<dyn Error>> {
    // Iterate across every line in the map
    let hm = input.iter().fold(HashMap::new(), |mut acc, l| {
        for pt in l.along() {
            let counter = acc.entry(pt).or_insert(0);
            *counter += 1;
        }
        acc
    });

    // This is for debugging
    #[cfg(test)]
    {
        // Print out the board, but only for our tests
        let max_x = input
            .iter()
            .map(|c| i32::max(c.start.x, c.end.x))
            .max()
            .unwrap();
        let max_y = input
            .iter()
            .map(|c| i32::max(c.start.y, c.end.y))
            .max()
            .unwrap();
        for y in 0..=max_y {
            for x in 0..=max_x {
                let pt = Coord { x, y };
                match hm.get(&pt) {
                    Some(&cnt) => print!("{}", cnt),
                    None => print!("."),
                }
            }
            println!();
        }
    }

    // Count the number of places where there is more than one vent
    Ok(hm.values().filter(|&&v| v > 1).count().try_into().unwrap())
}

pub fn main() -> Result<(), Box<dyn Error>> {
    // Parse each line of input into a Line struct
    let input: Vec<T> = read_file()?
        .iter()
        .filter_map(|l| Line::try_from(&**l).ok())
        .collect();
    println!("{}", part1(&input)?);
    println!("{}", part2(&input)?);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &[&str] = &[
        "0,9 -> 5,9",
        "8,0 -> 0,8",
        "9,4 -> 3,4",
        "2,2 -> 2,1",
        "7,0 -> 7,4",
        "6,4 -> 2,0",
        "0,9 -> 2,9",
        "3,4 -> 1,4",
        "0,0 -> 8,8",
        "5,5 -> 8,2",
    ];

    #[test]
    fn test_solve1() {
        let input: Vec<_> = INPUT
            .iter()
            .filter_map(|l| Line::try_from(&**l).ok())
            .collect();
        assert_eq!(part1(&input).unwrap(), 5);
    }

    #[test]
    fn test_solve2() {
        let input: Vec<_> = INPUT
            .iter()
            .filter_map(|l| Line::try_from(&**l).ok())
            .collect();
        assert_eq!(part2(&input).unwrap(), 12);
    }
}
