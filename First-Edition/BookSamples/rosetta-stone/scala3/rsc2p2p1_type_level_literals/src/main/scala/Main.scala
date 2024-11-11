import Automaton.*
import GoL.*
import Seeds.*

import scala.compiletime.*


object Literals:

  class Literal[M]

  trait Describe[T]:
    extension (t: T) def describe: String

  given ld[CH <: Char & Singleton](using ValueOf[CH]):
    Describe[Literal[CH]] with
    extension (t: Literal[CH]) def describe: String =
      s"${summon[ValueOf[CH]].value}"


  given id[I <: Int & Singleton](using ValueOf[I]):
    Describe[Literal[I]] with
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

  val literalChar = Literals.Literal['x']
  println(literalChar.describe)

  val literalInt = Literals.Literal[1]
  println(literalInt.describe)

  val literalBool = Literals.Literal[true]
  // Won't compile: not such instance
  // println(literalBool.describe)

  // DSL with literals test
  val glider1 = GoL.golAutomaton.wrap(mergeBoards(2, 2, emptyBoard(10, 10), DSL.glider()))
  val glider2 = iterateWorld(glider1, GoL.golAutomaton, 1)
  val output = GoL.golAutomaton.unwrap(glider2)
  DSL.displayBoard(output)
