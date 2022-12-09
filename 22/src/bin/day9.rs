use aoc22::read_file;
use std::{collections::HashSet, error::Error, ops::Add, ops::Sub};

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

const X: Point = Point { x: 1, y: 0 };
const Y: Point = Point { x: 0, y: 1 };

impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Sub for Point {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    R(i32),
    L(i32),
    U(i32),
    D(i32),
}

fn part1(input: &[Instruction]) -> Result<usize, Box<dyn Error>> {
    let mut head_pos = Point::new(0, 0);
    let mut tail_pos = Point::new(0, 0);
    let mut seen_pos = HashSet::new();
    seen_pos.insert(tail_pos);
    for &instr in input {
        let (head_move, d) = match instr {
            Instruction::R(d) => (Point::new(1, 0), d),
            Instruction::L(d) => (Point::new(-1, 0), d),
            Instruction::U(d) => (Point::new(0, 1), d),
            Instruction::D(d) => (Point::new(0, -1), d),
        };
        for _ in 0..d {
            head_pos = head_pos + head_move;

            let delta = head_pos - tail_pos;

            tail_pos = if delta.x >= 2 {
                head_pos - X
            } else if delta.x <= -2 {
                head_pos + X
            } else if delta.y >= 2 {
                head_pos - Y
            } else if delta.y <= -2 {
                head_pos + Y
            } else {
                tail_pos
            };
            seen_pos.insert(tail_pos);
        }
    }
    Ok(seen_pos.len())
}

fn part2(input: &[Instruction]) -> Result<usize, Box<dyn Error>> {
    let mut head_pos = Point::new(0, 0);
    let mut seen_pos = HashSet::new();
    let rope = &mut [Point::new(0, 0); 9];
    seen_pos.insert(rope[8]);
    for &instr in input {
        let (head_move, d) = match instr {
            Instruction::R(d) => (Point::new(1, 0), d),
            Instruction::L(d) => (Point::new(-1, 0), d),
            Instruction::U(d) => (Point::new(0, 1), d),
            Instruction::D(d) => (Point::new(0, -1), d),
        };
        for _ in 0..d {
            head_pos = head_pos + head_move;
            let delta = head_pos - rope[0];
            rope[0] = if delta.x >= 2 {
                head_pos - X
            } else if delta.x <= -2 {
                head_pos + X
            } else {
                rope[0]
            };
            rope[0] = if delta.y >= 2 {
                head_pos - Y
            } else if delta.y <= -2 {
                head_pos + Y
            } else {
                rope[0]
            };
            for i in 0..rope.len() - 1 {
                let prev = rope[i];
                let cur = rope[i + 1];
                let delta = prev - cur;
                rope[i + 1] = if delta.x >= 2 {
                    rope[i] - X
                } else if delta.x <= -2 {
                    rope[i] + X
                } else {
                    rope[i + 1]
                };
                rope[i + 1] = if delta.y >= 2 {
                    rope[i] - Y
                } else if delta.y <= -2 {
                    rope[i] + Y
                } else {
                    rope[i + 1]
                };
            }
            seen_pos.insert(rope[8]);
        }
    }
    Ok(seen_pos.len())
}

fn parse(input: &[&str]) -> Vec<Instruction> {
    input
        .iter()
        .map(|ln| {
            let mut ln = ln.split(' ');
            match ln.next().unwrap() {
                "R" => Instruction::R(ln.next().unwrap().parse::<i32>().unwrap()),
                "L" => Instruction::L(ln.next().unwrap().parse::<i32>().unwrap()),
                "U" => Instruction::U(ln.next().unwrap().parse::<i32>().unwrap()),
                "D" => Instruction::D(ln.next().unwrap().parse::<i32>().unwrap()),
                _ => unreachable!(),
            }
        })
        .collect()
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day9")?;
    let input: Vec<_> = parse(&binding.iter().map(|ln| &**ln).collect::<Vec<_>>());
    println!("Part1: {}", part1(&input)?);
    println!("Part2: {}", part2(&input)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{parse, part1, part2};

    const TEST_DATA: &str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

    const TEST_DATA2: &str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";
    #[test]
    pub fn test1() {
        let input = parse(&TEST_DATA.lines().map(|ln| &*ln).collect::<Vec<_>>());
        assert_eq!(13, part1(&input).unwrap());
    }

    #[test]
    pub fn test2() {
        let input = parse(&TEST_DATA.lines().map(|ln| &*ln).collect::<Vec<_>>());
        assert_eq!(1, part2(&input).unwrap());

        let input = parse(&TEST_DATA2.lines().map(|ln| &*ln).collect::<Vec<_>>());
        assert_eq!(36, part2(&input).unwrap());
    }
}
