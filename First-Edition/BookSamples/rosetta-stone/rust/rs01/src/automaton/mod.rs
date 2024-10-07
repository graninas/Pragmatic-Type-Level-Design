
pub mod gol;

use std::fmt;

// #[derive(Clone, Copy)]
#[derive(Copy)]
pub enum Cell {
  Alive,
  Dead
}

pub type Board = Vec<Vec<Cell>>;


pub fn make_empty_board(w: usize, h: usize) -> Board {
  vec![vec![Cell::Dead; w]; h]
}

pub fn merge_boards(x: usize, y: usize, base_board: Board, from_board: Board) -> Board {
  let mut new_board = base_board.clone();

  for i in 0..from_board.len() {
    for j in 0..from_board[i].len() {
      let px = i + x;
      let py = j + y;

      if px < new_board.len() && py < new_board[px].len() {
        new_board[px][py] = from_board[i][j];
      }
    }
  }

  return new_board;
}

pub fn iterate_world (
  step: impl Fn(Board) -> Board,
  n: u32,
  board: Board) -> Board {

  let mut new_board = board.clone();

  match n {
    0 => return new_board,
    _ => {
      for _i in 0..n {
        new_board = step(new_board);
    }},
  }

  return new_board;
}

pub fn get_neighbor_alive(x:usize, y:usize, board: &Board) -> u8 {
  let mut alives = 0;

  for i in -1..2 {
    for j in -1..2 {
      let xx = x as i32 + i;
      let yy = y as i32 + j;

      if xx >= 0 && yy >= 0 {
        alives += match board.get(xx as usize) {
          None => 0,
          Some(row) => match row.get(yy as usize) {
            None => 0,
            Some(cell) => match cell {
              Cell::Alive => 1,
              Cell::Dead => 0,
            },
          },
        }
      }
    }
  }

  return alives;
}




impl fmt::Display for Cell {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Cell::Alive => write!(f, "•"),
      Cell::Dead => write!(f, " "),
    }
  }
}


// TODO: use deriving instead of this (just for learning)
impl Clone for Cell {
  fn clone(&self) -> Self {
    return *self;
  }
}