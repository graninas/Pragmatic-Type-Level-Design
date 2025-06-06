import Automaton.*

object GoL:

  type GoLRule = "Game of Life"
  type GoLWorld = CellWorld[GoLRule]

  given golAutomaton: IAutomaton[GoLRule] with
    extension (world: GoLWorld) def step: GoLWorld =
      val newBoard = world.board.indices.map { x =>
        world.board(x).indices.map { y =>
          nextState(world.board, x, y)
        }.toVector
      }.toVector
      CellWorld[GoLRule](newBoard)

    extension (board: Board) def wrap: GoLWorld = CellWorld[GoLRule](board)
    extension (world: GoLWorld) def unwrap: Board = world.board

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

