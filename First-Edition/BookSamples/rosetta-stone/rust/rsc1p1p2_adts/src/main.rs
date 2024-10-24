
use std::fmt;

// ADT
pub enum Cell {
  Alive,
  Dead
}
pub type Board = Vec<Vec<Cell>>;

// Newtype ADT
pub struct GoL(pub Board);


// Usage

fn glider () -> Board {
  let glider: Board = vec!
    [ vec![Cell::Dead, Cell::Dead, Cell::Alive]
    , vec![Cell::Alive,Cell::Dead, Cell::Alive]
    , vec![Cell::Dead, Cell::Alive,Cell::Alive]
    ];
  return glider;
}

fn main() {
    let gol_glider = GoL(glider());

  let output = &gol_glider.0;

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

impl fmt::Display for Cell {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Cell::Alive => write!(f, "•"),
      Cell::Dead => write!(f, " "),
    }
  }
}
