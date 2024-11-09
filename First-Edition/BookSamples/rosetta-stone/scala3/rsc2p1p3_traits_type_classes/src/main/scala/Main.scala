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
  val glider1 = GoL.golAutomaton.wrap(mergeBoards(2, 2, emptyBoard(10, 10), glider()))
  val glider2 = iterateWorld(glider1, GoL.golAutomaton, 1)
  val output = GoL.golAutomaton.unwrap(glider2)

  displayBoard(output)

