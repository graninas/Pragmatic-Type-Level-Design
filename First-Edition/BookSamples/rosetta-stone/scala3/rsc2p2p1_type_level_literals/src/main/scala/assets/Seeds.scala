import Automaton.*

object Seeds:

  type SeedsRule = "Seeds"
  type SeedsWorld = CellWorld[SeedsRule]

  given seedsAutomaton: IAutomaton[SeedsRule] with
    extension (world: SeedsWorld) def step: SeedsWorld =
      val newBoard = world.board.indices.map { x =>
        world.board(x).indices.map { y =>
          nextState(world.board, x, y)
        }.toVector
      }.toVector
      CellWorld[SeedsRule](newBoard)

    extension (board: Board) def wrap: SeedsWorld = CellWorld[SeedsRule](board)
    extension (world: SeedsWorld) def unwrap: Board = world.board

  def nextState(board: Board, x: Int, y: Int): Cell =
    // TODO: implementation
    Dead


