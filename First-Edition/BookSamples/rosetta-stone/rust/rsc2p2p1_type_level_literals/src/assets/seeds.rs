use std::marker::PhantomData;
use crate::automaton;

use automaton::Board;
use automaton::CellWorld;
use automaton::Cell;
use automaton::IAutomaton;
use automaton::Born;
use automaton::Survived;

pub type Seeds = CellWorld<Born<1>, Survived<0>>;

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
  return CellWorld::CW {
    board,
    _marker1: PhantomData::<Born<1>>,
    _marker2: PhantomData::<Survived<0>>,
  };
}

impl IAutomaton for Seeds {
  fn step(self) -> Self {
    let (board, _marker1, _marker2) = match self {
      CellWorld::CW {
        board,
        _marker1,
        _marker2
      } => (seeds_step(board), _marker1, _marker2)
    };
    return CellWorld::CW{ board, _marker1, _marker2 };
  }

  fn unwrap(&self) -> &Board {
    let CellWorld::CW {board, ..} = self;
    return &board;
  }

  fn wrap(board: Board) -> Self {
    return CellWorld::CW {
      board,
    _marker1: PhantomData::<Born<1>>,
    _marker2: PhantomData::<Survived<0>>,
    };
  }
}

