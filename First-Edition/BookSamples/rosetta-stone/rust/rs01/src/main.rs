
mod automaton;
mod gol;

use automaton::Cell;
use automaton::Board;

fn glider () -> Board {
  let glider: Board = vec!
    [ vec![Cell::Dead, Cell::Dead, Cell::Alive]
    , vec![Cell::Alive,Cell::Dead, Cell::Alive]
    , vec![Cell::Dead, Cell::Alive,Cell::Alive]
    ];
  return glider;
}


fn main() {
  let glider1 = glider();
  let glider2 = gol::iterate_world(5, glider1);

    for x in glider2 {
      for y in x {
        println!("{}", y);
      }
    }
}
