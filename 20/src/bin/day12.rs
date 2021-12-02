use std::process::exit;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Expected filename");
        exit(1);
    }

    let filename = args.get(1).unwrap();
    let contents = fs::read_to_string(filename).expect("Something's wrong");

    let instructions: Vec<(char, i32)> = contents
        .split('\n')
        .map(|line| {
            (
                line.chars().nth(0).unwrap(),
                line.chars()
                    .skip(1)
                    .collect::<String>()
                    .parse::<i32>()
                    .unwrap(),
            )
        })
        .collect();

    part1(&instructions);
    part2(&instructions);
}

fn part1(instructions: &Vec<(char, i32)>) {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut theta = 0;
    for (direction, step) in instructions {
        match direction {
            'N' => y += step,
            'S' => y -= step,
            'E' => x += step,
            'W' => x -= step,
            'R' => theta += step,
            'L' => theta -= step,
            'F' => match theta.rem_euclid(360) {
                0 => x += step,
                90 => y -= step,
                180 => x -= step,
                270 => y += step,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
    println!("Part 1: |{}| + |{}| = {}", x, y, x.abs() + y.abs());
}

fn part2(instructions: &Vec<(char, i32)>) {
    let mut waypoint_x = 10;
    let mut waypoint_y = 1;
    let mut ship_x = 0;
    let mut ship_y = 0;

    for (dir, step) in instructions {
        match dir {
            'N' => waypoint_y += step,
            'S' => waypoint_y -= step,
            'E' => waypoint_x += step,
            'W' => waypoint_x -= step,
            'L' => {
                let (x, y) = rotate_waypoint(*step, (waypoint_x, waypoint_y));
                waypoint_x = x;
                waypoint_y = y;
            }
            'R' => {
                let (x, y) = rotate_waypoint(360 - *step, (waypoint_x, waypoint_y));
                waypoint_x = x;
                waypoint_y = y;
            }
            'F' => {
                ship_x += waypoint_x * step;
                ship_y += waypoint_y * step;
            }
            _ => unimplemented!(),
        }
    }
    println!(
        "Part 2: |{}| + |{}| = {}",
        ship_x,
        ship_y,
        ship_x.abs() + ship_y.abs()
    )
}

fn rotate_waypoint(deg: i32, waypoint: (i32, i32)) -> (i32, i32) {
    match deg {
        90 => (-waypoint.1, waypoint.0),
        180 => (-waypoint.0, -waypoint.1),
        270 => (waypoint.1, -waypoint.0),
        _ => unreachable!(),
    }
}
