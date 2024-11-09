object Automaton:

  sealed trait Cell
  case object Alive extends Cell
  case object Dead extends Cell

  type Board = Vector[Vector[Cell]]

  trait IAutomaton:
    def step(): IAutomaton
    def unwrap(): Board


  def iterateWorld(world: IAutomaton, n: Int): IAutomaton =
    if n <= 0 then world
    else iterateWorld(world.step(), n - 1)

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
