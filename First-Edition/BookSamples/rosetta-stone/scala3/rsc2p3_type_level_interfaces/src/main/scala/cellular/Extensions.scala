package cellular
package extensions

import cellular.typelevel._
import cellular.language._


// Parameterized implementation type, option 1: associated types
sealed trait StateImpl:
  type Name <: String
  type Index <: Int

type State[N <: String, I <: Int] =
  IStateObj.MkState[StateImpl { type Name = N; type Index = I }]


// Parameterized implementation type, option 2: type parameters
case class NeighborsCountImpl[
  S <: IState,
  Cnts <: IntList]()

type NeighborsCount[S <: IState, Cnts <: IntList] =
  ICellConditionObj.MkCellCondition[
    NeighborsCountImpl[S, Cnts]]


case class StateTransitionImpl[
  From <: IState,
  To <: IState,
  Conds <: ICellCondition]()

type StateTransition[
  From <: IState,
  To <: IState,
  Conds <: ICellCondition] =
  IStateTransitionObj.MkStateTransition[
    StateTransitionImpl[From, To, Conds]]


case class AdjacentsLvlImpl[
  Lvl <: Int & Singleton]()

type AdjacentsLvl[Lvl <: Int & Singleton] =
  INeighborhoodObj.MkNeighborhood[
    AdjacentsLvlImpl[Lvl]]


case class StepImpl[
  DefState <: IState,
  Transitions <: HList[IStateTransition]]()

type Step[
  DefState <: IState,
  Transitions <: HList[IStateTransition]] =
  IStepObj.MkStep[
    StepImpl[DefState, Transitions]]



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
  IRuleObj.MkRule[
    RuleImpl[Name, Code, Neighborhood, Step]]
