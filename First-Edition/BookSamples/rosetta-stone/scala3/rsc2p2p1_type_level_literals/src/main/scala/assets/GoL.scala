import Automaton.*

object GoL:

  class GoLRule extends BSRule

  type GoL = CellWorld[GoLRule, Placeholder]
  // type Seeds = CellWorld[Born[1], Survived[0]]

  given golAutomaton: IAutomaton[GoL] with
    extension (world: GoL) def step: GoL =
      val newBoard = world.board.indices.map { x =>
        world.board(x).indices.map { y =>
          nextState(world.board, x, y)
        }.toVector
      }.toVector
      val newGoL: GoL = CellWorld[GoLRule, Placeholder](newBoard)
      newGoL

    extension (board: Board) def wrap: GoL = CellWorld[GoLRule, Placeholder](board)
    extension (world: GoL) def unwrap: Board = world.board

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

