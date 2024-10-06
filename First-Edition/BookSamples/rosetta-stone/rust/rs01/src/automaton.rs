
use std::fmt;


pub enum Cell {
  Alive,
  Dead
}

pub type Board = Vec<Vec<Cell>>;

impl fmt::Display for Cell {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Cell::Alive => write!(f, "A"),
      Cell::Dead => write!(f, "D"),
    }
  }
}
