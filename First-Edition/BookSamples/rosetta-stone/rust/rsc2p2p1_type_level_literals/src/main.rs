mod automaton;
mod assets;

use automaton::Cell;
use automaton::Board;
use automaton::IAutomaton;

#[allow(unused_imports)]
use assets::seeds::Seeds;

// This example shows that type-level literals can be converted
// to values directly.
struct LiteralsTest<const CH: char, const B: bool, const I: i32>;

trait Description {
  fn describe() -> String;
}

impl<const CH: char, const B: bool, const I: i32>
  Description for LiteralsTest<CH, B, I> {
  fn describe() -> String {
    format!("char: {}, bool: {}, int: {}", CH, B, I)
  }
}

// Cellular automaton example
fn glider () -> Board {
  let glider: Board = vec!
    [ vec![Cell::Dead, Cell::Dead, Cell::Alive]
    , vec![Cell::Alive,Cell::Dead, Cell::Alive]
    , vec![Cell::Dead, Cell::Alive,Cell::Alive]
    ];
  return glider;
}


fn main() {
  println!("{}", LiteralsTest::<'a', true, 10>::describe());

  let glider1 = automaton::merge_boards(2, 2, automaton::make_empty_board(10, 10), glider());
  let gol = assets::gol::make_gol(glider1);
  let gol_glider2 = automaton::iterate_world(1, gol);

  let output = gol_glider2.unwrap();

  let mut strs = String::from("");
  for row in output {
    let mut str_row = String::from("");
    for cell in row {
      str_row.push_str(&cell.to_string());
    }
    strs.push_str("\n");
    strs.push_str(&str_row);
  }

  println!("{}", strs);


  let glider2 = automaton::merge_boards(2, 2, automaton::make_empty_board(10, 10), glider());
  let seeds = assets::seeds::make_seeds(glider2);
  let seeds_world = automaton::iterate_world(1, seeds);

  let output_seeds = seeds_world.unwrap();

  strs = String::from("");

  for row in output_seeds {
    let mut str_row = String::from("");
    for cell in row {
      str_row.push_str(&cell.to_string());
    }
    strs.push_str("\n");
    strs.push_str(&str_row);
  }

  println!("{}", strs);
}


