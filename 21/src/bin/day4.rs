use aoc21::{read_file, ProblemError};
use std::error::Error;

type T = (Vec<i32>, Vec<Board>);

#[derive(Debug, Clone, Copy)]
struct Board {
    board: [[Option<i32>; 5]; 5],
}

impl From<Vec<Vec<i32>>> for Board {
    fn from(board_data: Vec<Vec<i32>>) -> Self {
        let mut board: [[Option<i32>; 5]; 5] = [[None; 5]; 5];
        for i in 0..5 {
            for j in 0..5 {
                board[i][j] = Some(board_data[i][j]);
            }
        }
        Board { board }
    }
}

fn compute_board_score(b: &Board) -> i32 {
    b.board
        .iter()
        .map(|r| r.iter().filter_map(|&e| e).sum::<i32>())
        .sum()
}

fn board_win(b: &Board) -> Option<i32> {
    for i in 0..5 {
        if b.board[i][i].is_none() {
            if b.board[i].iter().all(|e| e.is_none()) {
                return Some(compute_board_score(b));
            }
            if b.board.iter().all(|e| e[i].is_none()) {
                return Some(compute_board_score(b));
            }
        }
    }
    None
}

fn play_move(b: &mut Board, num: i32) {
    for i in 0..5 {
        for j in 0..5 {
            if let Some(p) = b.board[i][j] {
                if p == num {
                    b.board[i][j] = None;
                }
            }
        }
    }
}

fn part1(input: &T) -> Result<i32, Box<dyn Error>> {
    let mut local_boards = input.1.clone();
    for &num in &input.0 {
        if let Some(score) = local_boards.iter_mut().find_map(|b| {
            play_move(b, num);
            board_win(b)
        }) {
            return Ok(score * num);
        }
    }
    Err(Box::new(ProblemError {}))
}

fn part2(input: &T) -> Result<i32, Box<dyn Error>> {
    let mut local_boards = input.1.clone();
    let mut last_score = 0;
    for &num in &input.0 {
        let idx_scores: Vec<_> = local_boards
            .iter_mut()
            .enumerate()
            .filter_map(|(idx, b)| {
                play_move(b, num);
                board_win(b).map(|score| (idx, score))
            })
            .collect();

        for &idx in idx_scores.iter().rev() {
            local_boards.remove(idx.0);
        }
        if let Some(&a) = idx_scores.iter().last() {
            last_score = a.1 * num;
        }
    }
    Ok(last_score)
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents = read_file()?;
    let contents = contents;

    let rand_nums = contents[0]
        .split(',')
        .filter_map(|s| s.parse::<i32>().ok())
        .collect();

    let boards_data = &contents[1..];

    let boards: Vec<_> = boards_data
        .chunks(5)
        .map(|board| {
            board
                .iter()
                .map(|l| {
                    l.split(' ')
                        .filter(|e| !e.is_empty())
                        .map(|s| s.parse::<i32>().unwrap())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .map(Board::from)
        .collect();

    let input = (rand_nums, boards);

    println!("Part1: {}", part1(&input)?);
    println!("Part2: {}", part2(&input)?);
    Ok(())
}
