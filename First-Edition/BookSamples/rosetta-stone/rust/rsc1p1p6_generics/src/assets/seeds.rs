use crate::automaton;

use automaton::Board;

#[allow(unused_imports)]
use automaton::Cell;

#[derive(Clone)]
#[allow(dead_code)]
pub struct Seeds(pub Board);

#[allow(dead_code)]
pub fn seeds_step(seeds: &Seeds) -> Seeds {
  let new_board = seeds.0.clone();

  // Seeds logic here

  return Seeds(new_board);
}

