
use std::fmt;

// #[derive(Clone, Copy)]
#[derive(Copy)]
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


// TODO: use deriving instead of this (just for learning)
impl Clone for Cell {
  fn clone(&self) -> Self {
    return *self;
  }
}
