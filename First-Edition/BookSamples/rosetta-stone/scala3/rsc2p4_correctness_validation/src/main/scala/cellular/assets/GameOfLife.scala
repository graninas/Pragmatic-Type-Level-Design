package cellular
package assets

import graninas.typelevel._
import cellular.language._
import cellular.extensions._

object GoL {

  type A = State["Alive", 1]
  type D = State["Dead", 0]

  type Unknown = State["Unknown", 2]
  type Balrog = State["Balrog", 3]

  type Neighbors3  = NeighborsCount[A, 3 :: IN]
  type Neighbors23 = NeighborsCount[A, 2 :: 3 :: IN]

  type Trans1 = StateTransition[D, A, Neighbors3]
  type Trans2 = StateTransition[A, A, Neighbors23]

  type Transitions = HL[IStateTransition] :+ Trans2 :+ Trans1
  type Transitions2 = Trans1 +: Trans2 +: Nil[IStateTransition]

  type Transitions3 =
    Cons[IStateTransition, Trans1,
      Cons[IStateTransition, Trans2,
        Nil[IStateTransition]]]

  type GoLStep = Step[D, Transitions]

  type States = HL[IState] :+ A :+ D

  type GoLRule = Rule[
    "Game of Life",
    "gol",
    AdjacentsLvl[1],
    GoLStep]

}
