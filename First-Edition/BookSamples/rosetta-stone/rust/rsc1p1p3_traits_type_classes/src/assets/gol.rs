
use crate::automaton;

use automaton::Board;
use automaton::Cell;
use automaton::Automaton;

#[derive(Clone)]
pub struct GoL(pub Board);

fn gol_step(board: &Board) -> Board {
  let mut new_board = board.clone();

  for (i, row) in board.iter().enumerate() {
    for (j, cell) in row.iter().enumerate() {
      let nh = automaton::get_neighbor_alive(i, j, &board);

      new_board[i][j] = match (cell, nh) {
        (Cell::Alive, 2) => Cell::Alive,
        (Cell::Alive, 3) => Cell::Alive,
        (Cell::Dead,  3) => Cell::Alive,
        _ => Cell::Dead,
      }
    }
  }

  return new_board;
}

impl Automaton for GoL {
  fn step(&self) -> Self {
    return GoL(gol_step(&self.0));
  }
}
