use std::marker::PhantomData;
use crate::automaton;

use automaton::Board;
use automaton::CellWorld;
use automaton::Cell;
use automaton::IAutomaton;

pub enum GoLRule {}

pub type GoL = CellWorld<GoLRule>;

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

pub fn make_gol(board: &Board) -> GoL {
  return CellWorld::CW{ board: board.to_vec(), _marker: PhantomData::<GoLRule> };
}

impl IAutomaton for GoL {
  fn step(&self) -> Self {
    let new_board = match self {
      CellWorld::CW {board, ..} => gol_step(&board)
    };
    return CellWorld::CW{ board: new_board, _marker: PhantomData::<GoLRule> };
  }
}
