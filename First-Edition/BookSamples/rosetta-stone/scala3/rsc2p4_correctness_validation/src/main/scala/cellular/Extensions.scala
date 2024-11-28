package cellular
package extensions

import graninas.typelevel._
import cellular.language._


case class Literal[M]()


// Parameterized implementation type, option 1: type parameters

case class StateImpl[
  Name <: String & Singleton,
  Idx <: Int & Singleton]()

type State[Name <: String & Singleton, I <: Int & Singleton] =
  MkState[StateImpl[Name, I]]


// Parameterized implementation type, option 2: associated types
// TODO: double-check how to interpret it
sealed trait NeighborsCountImpl:
  type State <: IState
  type Counts <: IntList

type NeighborsCount[S <: IState, Cnts <: IntList] =
  MkCellCondition[NeighborsCountImpl {
    type State = S
    type Counts = Cnts
  }]


case class StateTransitionImpl[
  From <: IState,
  To <: IState,
  Conds <: ICellCondition]()

type StateTransition[
  From <: IState,
  To <: IState,
  Conds <: ICellCondition] =
  MkStateTransition[StateTransitionImpl[From, To, Conds]]


case class AdjacentsLvlImpl[
  Lvl <: Int & Singleton]()

type AdjacentsLvl[Lvl <: Int & Singleton] =
  MkNeighborhood[AdjacentsLvlImpl[Lvl]]


case class StepImpl[
  DefState <: IState,
  Transitions <: HList[IStateTransition]]()

type Step[
  DefState <: IState,
  Transitions <: HList[IStateTransition]] =
  MkStep[StepImpl[DefState, Transitions]]


case class RuleImpl[
  Name <: String & Singleton,
  Code <: String & Singleton,
  Neighborhood <: INeighborhood,
  Step <: IStep]()

type Rule[
  Name <: String & Singleton,
  Code <: String & Singleton,
  Neighborhood <: INeighborhood,
  Step <: IStep] =
  MkRule[RuleImpl[Name, Code, Neighborhood, Step]]
