package cellular
package assets

import graninas.typelevel._
import cellular.language._
import cellular.extensions._


object Seeds {

  type A = State["Alive", 1]
  type D = State["Dead", 0]

  type Neighbors2 = NeighborsCount[D, 2 :: IN]

  type Trans1 = StateTransition[D, A, Neighbors2]

  type Transitions =
    Cons[IStateTransition, Trans1,
      Nil[IStateTransition]]


  type SeedsStep = Step[D, Transitions]

  type States = Cons[IState, A, Cons[IState, D, Nil[IState]]]

  type SeedsRule = Rule[
    "Seeds",
    "seeds",
    AdjacentsLvl[1],
    SeedsStep]

}
