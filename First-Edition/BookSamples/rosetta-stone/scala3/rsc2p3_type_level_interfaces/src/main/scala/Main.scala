import scala.compiletime._

sealed trait HList[Kind]
case class Nil[Kind]() extends HList[Kind]
case class Cons[Kind, T <: Kind, Tail <: HList[Kind]]() extends HList[Kind]

///////////////////


sealed trait IState

object IStateObj:
  type StateWrapper[A] <: IState
  type MkState[A] = StateWrapper[A]

sealed trait StateImpl:
  type Name <: String
  type Index <: Int

type State[N <: String, I <: Int] =
  IStateObj.MkState[StateImpl { type Name = N; type Index = I }]

case class RGBImpl[R <: Int, G <: Int, B <: Int]()
type RGB[R <: Int, G <: Int, B <: Int] =
  IStateObj.MkState[RGBImpl[R, G, B]]


sealed trait IStateTransition


object IStateTransitionObj:
  type StateTransitionWrapper[A] <: IStateTransition
  type MkStateTransition[A] = StateTransitionWrapper[A]


case class StateTransitionImpl[From <: IState, To <: IState]()

type StateTransition[F <: IState, T <: IState] =
  IStateTransitionObj.MkStateTransition[
    StateTransitionImpl[F, T]]

type A = State["Alive", 1]
type D = State["Dead", 0]

type Trans1 = StateTransition[A, D]
type Trans2 = StateTransition[D, RGB[2,2,2]]


type States = Cons[IState, A, Nil[IState]]

type Transitions = Cons[IStateTransition, Trans1,
  Cons[IStateTransition, Trans2, Nil[IStateTransition]]]


@main def hello(): Unit =
  println("Hello world!")
