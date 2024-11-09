import Automaton.*

object GoL:

  case class GoL(board: Board) extends IAutomaton {
    def step(): GoL =
      val newBoard = board.indices.map { x =>
        board(x).indices.map { y =>
          nextState(board, x, y)
        }.toVector
      }.toVector
      GoL(newBoard)
    def unwrap(): Board = board
  }

  def nextState(board: Board, x: Int, y: Int): Cell = {
    val neighbors = for {
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j != 0
      if board.isDefinedAt(x + i) && board(x + i).isDefinedAt(y + j)
    } yield board(x + i)(y + j)

    val aliveNeighbors = neighbors.count(_ == Alive)

    board(x)(y) match {
      case Alive if aliveNeighbors < 2 || aliveNeighbors > 3 => Dead
      case Alive => Alive
      case Dead if aliveNeighbors == 3 => Alive
      case Dead => Dead
    }
  }

