

use super::Board;

pub fn iterate_world (
  step: impl Fn(Board) -> Board,
  n: u32,
  board: Board) -> Board {

  match n {
    0 => board,
    _ => step(board),
  }

}

