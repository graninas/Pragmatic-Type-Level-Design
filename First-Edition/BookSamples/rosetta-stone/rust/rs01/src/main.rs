
mod automaton;

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
  let glider1 = automaton::merge_boards(2, 2, automaton::make_empty_board(10, 10), glider());
  let glider2 = automaton::iterate_world(automaton::gol::gol_step, 1, glider1);

  let output = &glider2;

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
}
