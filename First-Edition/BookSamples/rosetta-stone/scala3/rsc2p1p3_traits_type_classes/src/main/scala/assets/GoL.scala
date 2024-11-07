import Automaton.*

object GoL:

  case class GoL(board: Board) extends Automaton {
    def step(): GoL =
      GoL(board)
  }





// class CellularAutomaton(val board: Board) extends Automaton {
//   def step()(using rule: AutomatonRule): CellularAutomaton = {
//     val newBoard = board.indices.map { x =>
//       board(x).indices.map { y =>
//         rule.nextState(board, x, y)
//       }.toVector
//     }.toVector
//     new CellularAutomaton(newBoard)
//   }
// }
