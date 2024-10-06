

use super::Board;

pub fn iterate_world (
  // step: impl Fn(Board) -> Board,
  n: u32,
  board: Board) -> Board {
  match n {
    0 => board,
    _ => board,
  }
}


// golStep :: Board -> Board
// golStep = error "Not implemented"

// iterateWorld :: Int -> Board -> Board
// iterateWorld n board | n == 0 = board
// iterateWorld n board | n > 0 =
//   head (drop n (iterate golStep board))
// iterateWorld _ _ = error "Invalid iteration count"
