object Automaton:

  sealed trait Cell
  case object Alive extends Cell
  case object Dead extends Cell

  type Board = Vector[Vector[Cell]]

  case class CellWorld[Id <: String & Singleton](board: Board)

  trait IAutomaton[Id <: String & Singleton]:
    extension (world: CellWorld[Id]) def step: CellWorld[Id]
    extension (board: Board) def wrap: CellWorld[Id]
    extension (world: CellWorld[Id]) def unwrap: Board

  def iterateWorld[Id <: String & Singleton](
    world: CellWorld[Id],
    automaton: IAutomaton[Id],
    n: Int): CellWorld[Id] =
    if n <= 0 then world
    else iterateWorld(automaton.step(world), automaton, n - 1)

// Board functions

  def emptyBoard(w: Int, h: Int): Board =
    Vector.fill(w, h)(Dead)

  def mergeBoards(
    x: Int,
    y: Int,
    baseBoard: Board,
    fromBoard: Board): Board =

    baseBoard.zipWithIndex.map { (row, i) =>
      row.zipWithIndex.map { (cell, j) =>
        val px = i - x
        val py = j - y

        if px >= 0 && px < fromBoard.length && py >= 0 && py < fromBoard(px).length then
          fromBoard(px)(py)
        else
          cell
      }
    }


  // Implicit conversion for Cell to string representation
  given Conversion[Cell, String] with
    def apply(cell: Cell): String = cell match
      case Alive => "•"
      case Dead => " "
