object GameOfLife:

  sealed trait Cell
  case object Alive extends Cell
  case object Dead extends Cell

  // enum Cell:
  //   case Alive
  //   case Dead

  type Board = Vector[Vector[Cell]]

  case class GoL(board: Board)

  def glider(): Board =
    Vector(
      Vector(Dead, Dead, Alive),
      Vector(Alive, Dead, Alive),
      Vector(Dead, Alive, Alive)
    )

  // Implicit conversion for Cell to string representation
  given Conversion[Cell, String] with
    def apply(cell: Cell): String = cell match
      case Alive => "•"
      case Dead => " "

  def displayBoard(args: Array[String]): Unit =
    val golGlider = GoL(glider())
    val output = golGlider.board

    val strBuilder = new StringBuilder()
    for row <- output do
      val rowStr = row.map(cell => cell: String).mkString
      strBuilder.append("\n").append(rowStr)

    println(strBuilder.toString)

  def patternMatchDemo =
    Dead match
      case Alive => println("•")
      case Dead => println(" ")

@main def run(): Unit =
  GameOfLife.displayBoard(Array())
