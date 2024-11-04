object GameOfLife:

  // ADT for Cell
  enum Cell:
    case Alive
    case Dead

  // Type alias for Board
  type Board = Vector[Vector[Cell]]

  // Wrapper class for GoL
  case class GoL(board: Board)

  def glider(): Board =
    Vector(
      Vector(Cell.Dead, Cell.Dead, Cell.Alive),
      Vector(Cell.Alive, Cell.Dead, Cell.Alive),
      Vector(Cell.Dead, Cell.Alive, Cell.Alive)
    )

  // Implicit conversion for Cell to string representation
  given Conversion[Cell, String] with
    def apply(cell: Cell): String = cell match
      case Cell.Alive => "•"
      case Cell.Dead => " "

  def displayBoard(args: Array[String]): Unit =
    val golGlider = GoL(glider())
    val output = golGlider.board

    val strBuilder = new StringBuilder()
    for row <- output do
      val rowStr = row.map(cell => cell: String).mkString
      strBuilder.append("\n").append(rowStr)

    println(strBuilder.toString)

@main def run(): Unit =
  GameOfLife.displayBoard(Array())
