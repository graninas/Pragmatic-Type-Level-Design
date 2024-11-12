import Automaton.*
import GoL.*
import Seeds.*

import scala.compiletime.*


class Literal[M]

trait Description[T]:
  extension (t: T) def describe: String

given chDescr[CH <: Char & Singleton](using ValueOf[CH]):
  Description[Literal[CH]] with
  extension (t: Literal[CH]) def describe: String =
    s"${summon[ValueOf[CH]].value}"


given intDescr[I <: Int & Singleton](using ValueOf[I]):
  Description[Literal[I]] with
  extension (t: Literal[I]) def describe: String =
    s"${summon[ValueOf[I]].value}"


object DSL:
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

  // Literals test

  val literalChar = Literal['x']
  val literalInt = Literal[1]

  println(literalChar.describe)             // prints x
  println(literalInt.describe)              // prints 1

  val literalBool = Literal[true]
  // Won't compile: not such instance
  // println(literalBool.describe)

  // DSL with literals test
  val glider1 = GoL.golAutomaton.wrap(mergeBoards(2, 2, emptyBoard(10, 10), DSL.glider()))
  val glider2 = iterateWorld(glider1, GoL.golAutomaton, 1)
  val output = GoL.golAutomaton.unwrap(glider2)
  DSL.displayBoard(output)
