
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

fn get_neighbor_alive(x:usize, y:usize, board: &Board) -> u8 {
  let mut alives = 0;

  for i in -1..2 {
    for j in -1..2 {
      let xx = x as i32 + i;
      let yy = y as i32 + j;

      if xx >= 0 && yy >= 0 {
        alives += match board.get(xx as usize) {
          None => 0,
          Some(row) => match row.get(yy as usize) {
            None => 0,
            Some(cell) => match cell {
              Cell::Alive => 1,
              Cell::Dead => 0,
            },
          },
        }
      }
    }
  }

  return alives;
}

fn gol_step(board: Board) -> Board {
  let mut new_board = board.clone();

  for (i, row) in board.iter().enumerate() {
    for (j, cell) in row.iter().enumerate() {
      let nh = get_neighbor_alive(i, j, &board);

      new_board[i][j] = match (cell, nh) {
        (Cell::Alive, 2) => Cell::Alive,
        (Cell::Alive, 3) => Cell::Alive,
        (Cell::Dead,  3) => Cell::Alive,
        _ => Cell::Dead,
      }
    }
  }

  return new_board;
}


fn main() {
  let glider1 = glider();
  let glider2 = gol::iterate_world(gol_step, 5, glider1);

    for x in glider2 {
      for y in x {
        println!("{}", y);
      }
    }
}
