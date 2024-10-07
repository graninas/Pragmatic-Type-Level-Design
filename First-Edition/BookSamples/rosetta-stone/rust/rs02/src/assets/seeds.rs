use crate::automaton;

use automaton::Board;

#[allow(unused_imports)]
use automaton::Cell;
use automaton::Automaton;

#[derive(Clone)]
pub struct Seeds(pub Board);

fn seeds_step(board: &Board) -> Board {
  let new_board = board.clone();

  // Seeds logic here

  return new_board;
}

impl Automaton for Seeds {
  fn step(&self) -> Self {
    return Seeds(seeds_step(&self.0));
  }
}
