
use crate::automaton;

use automaton::Board;
use automaton::Cell;

#[derive(Clone)]
pub struct GoL(pub Board);

pub fn gol_step(gol: &GoL) -> GoL {
  let mut new_board = gol.0.clone();

  for (i, row) in gol.0.iter().enumerate() {
    for (j, cell) in row.iter().enumerate() {
      let nh = automaton::get_neighbor_alive(i, j, &gol.0);

      new_board[i][j] = match (cell, nh) {
        (Cell::Alive, 2) => Cell::Alive,
        (Cell::Alive, 3) => Cell::Alive,
        (Cell::Dead,  3) => Cell::Alive,
        _ => Cell::Dead,
      }
    }
  }

  return GoL(new_board);
}
