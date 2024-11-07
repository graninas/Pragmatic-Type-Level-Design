import Automaton.*
import GoL.*

def glider(): Board =
  Vector(
    Vector(Dead, Dead, Alive),
    Vector(Alive, Dead, Alive),
    Vector(Dead, Alive, Alive)
  )

def displayBoard(board: Board): Unit =
  val strBuilder = new StringBuilder()

  for row <- board do
    val rowStr = row.map(cell => cell: String).mkString
    strBuilder.append("\n").append(rowStr)

  println(strBuilder.toString)


@main def run(): Unit =
  val glider1 = GoL(mergeBoards(2, 2, emptyBoard(10, 10), glider()))
  val glider2 = iterateWorld(using glider1, 1)
  val output = glider2.getBoard()

  displayBoard(output)

