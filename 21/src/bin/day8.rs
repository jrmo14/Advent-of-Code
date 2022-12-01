use aoc21::read_file;
use std::error::Error;

// Brian Kernighan's bit counter
fn bit_count(mut n: u8) -> i32 {
    let mut count = 0;
    while n != 0 {
        n &= n - 1;
        count += 1;
    }
    count
}

// Count 1, 4, 7, 8
fn part1(input: &[(Vec<u8>, Vec<u8>)]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|(wiring_samples, output_samples)| {
            let mut one = 0; // 2 set
            let mut four = 0; // 4 set
            let mut seven = 0; // 3 set
            let mut eight = 0; // 7 set
            for &sample in wiring_samples {
                match bit_count(sample) {
                    2 => one = sample,
                    4 => four = sample,
                    3 => seven = sample,
                    7 => eight = sample,
                    _ => {}
                }
            }
            let mut count = 0;
            for &sample in output_samples {
                if one == sample || four == sample || seven == sample || eight == sample {
                    count += 1;
                }
            }
            count
        })
        .sum())
}

fn part2(input: &[(Vec<u8>, Vec<u8>)]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|(wiring_samples, output_samples)| {
            let mut wire2num = [0u8; 256];
            let mut num2wire = [0u8; 10];

            let mut wiring_copy = wiring_samples.clone();
            // For ez just go back again
            // Probably some const time way to do it 1 pass, this is fast enough
            // typically takes 2-3 passes
            while !wiring_copy.iter().all(|&e| e == 0) {
                for sample in &mut wiring_copy {
                    if *sample == 0 {
                        continue;
                    }
                    match bit_count(*sample) {
                        2 => {
                            wire2num[*sample as usize] = 1;
                            num2wire[1] = *sample;
                            *sample = 0;
                        }
                        4 => {
                            wire2num[*sample as usize] = 4;
                            num2wire[4] = *sample;
                            *sample = 0;
                        }
                        3 => {
                            wire2num[*sample as usize] = 7;
                            num2wire[7] = *sample;
                            *sample = 0;
                        }
                        7 => {
                            wire2num[*sample as usize] = 8;
                            num2wire[8] = *sample;
                            *sample = 0;
                        }
                        6 => {
                            if num2wire[4] != 0 && num2wire[7] != 0 {
                                if num2wire[4] | num2wire[7] | *sample == *sample {
                                    wire2num[*sample as usize] = 9;
                                    num2wire[9] = *sample;
                                    *sample = 0;
                                } else if num2wire[7] | *sample == *sample {
                                    wire2num[*sample as usize] = 0;
                                    num2wire[0] = *sample;
                                    *sample = 0;
                                } else {
                                    wire2num[*sample as usize] = 6;
                                    num2wire[6] = *sample;
                                    *sample = 0;
                                }
                            }
                        }
                        5 => {
                            if num2wire[1] != 0 && num2wire[9] != 0 {
                                if *sample | num2wire[1] == *sample {
                                    wire2num[*sample as usize] = 3;
                                    num2wire[3] = *sample;
                                    *sample = 0;
                                } else if *sample | num2wire[9] == num2wire[9] {
                                    wire2num[*sample as usize] = 5;
                                    num2wire[5] = *sample;
                                    *sample = 0;
                                } else {
                                    wire2num[*sample as usize] = 2;
                                    num2wire[2] = *sample;
                                    *sample = 0;
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            output_samples
                .iter()
                .map(|&wiring| wire2num[wiring as usize] as i32)
                .reduce(|acc, i| acc * 10 + i)
                .unwrap()
        })
        .sum())
}
pub fn main() -> Result<(), Box<dyn Error>> {
    let generate_bitmask = |smpl: &str| {
        smpl.chars()
            .map(|c| 1 << (c as u8 - b'a'))
            .reduce(|accum, item| accum | item)
            .unwrap()
    };
    let input: Vec<_> = read_file()?
        .iter()
        .map(|line| {
            let mut lsplit = line.split(" | ");
            let wiring_samples = lsplit
                .next()
                .unwrap()
                .split(' ')
                .map(generate_bitmask)
                .collect::<Vec<u8>>();
            let output_samples = lsplit
                .next()
                .unwrap()
                .split(' ')
                .map(generate_bitmask)
                .collect::<Vec<u8>>();
            (wiring_samples, output_samples)
        })
        .collect();
    println!("{}", part1(&input)?);
    println!("{}", part2(&input)?);

    Ok(())
}
