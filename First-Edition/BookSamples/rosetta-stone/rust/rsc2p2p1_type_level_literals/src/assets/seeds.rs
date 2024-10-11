use std::marker::PhantomData;
use crate::automaton;

use automaton::Board;
use automaton::CellWorld;
use automaton::Cell;
use automaton::IAutomaton;

pub enum SeedsRule {}

pub type Seeds = CellWorld<SeedsRule>;

fn seeds_step(board: &Board) -> Board {
  let new_board = board.clone();

  // Seeds logic here

  return new_board;
}

impl IAutomaton for Seeds {
  fn step(&self) -> Self {
    let new_board = match self {
      CellWorld::CW {board, ..} => seeds_step(&board)
    };
    return CellWorld::CW{ board: new_board, _marker: PhantomData::<SeedsRule> };
  }
}
