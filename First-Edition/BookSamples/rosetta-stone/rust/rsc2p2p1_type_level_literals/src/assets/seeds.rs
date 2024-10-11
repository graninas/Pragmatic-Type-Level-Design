use std::marker::PhantomData;
use crate::automaton;

use automaton::Board;
use automaton::CellWorld;
use automaton::Cell;
use automaton::IAutomaton;

pub enum SeedsRule {}

pub type Seeds = CellWorld<SeedsRule>;

fn seeds_step(board: Board) -> Board {
  let mut new_board = board.clone();

  for (i, row) in board.iter().enumerate() {
    for (j, cell) in row.iter().enumerate() {
      let nh = automaton::get_neighbor_alive(i, j, &board);

      new_board[i][j] = match (cell, nh) {
        (Cell::Dead,  2) => Cell::Alive,
        _ => Cell::Dead,
      }
    }
  }

  return new_board;
}

#[allow(dead_code)]
pub fn make_seeds(board: Board) -> Seeds {
  return CellWorld::CW{ board, _marker: PhantomData::<SeedsRule> };
}

impl IAutomaton for Seeds {
  fn step(self) -> Self {
    let (board, _marker) = match self {
      CellWorld::CW {board, _marker} => (seeds_step(board), _marker)
    };
    return CellWorld::CW{ board, _marker };
  }

  fn unwrap(&self) -> &Board {
    let CellWorld::CW {board, _marker} = self;
    return &board;
  }

  fn wrap(board: Board) -> Self {
    return CellWorld::CW { board, _marker: PhantomData::<SeedsRule> };
  }
}

