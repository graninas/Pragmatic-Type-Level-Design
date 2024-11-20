package cellular
package introspection

import graninas.typelevel._
import cellular.language._
import cellular.extensions._

import scala.compiletime.*

object Introspection {

  case class Introspect()

  given nhIntrospect
    [Lvl <: Int & Singleton]
    (using ValueOf[Lvl]):
    Eval[(Introspect,
         Proxy[AdjacentsLvl[Lvl]]), String] with
    extension (t:
      (Introspect, Proxy[AdjacentsLvl[Lvl]])) def eval: String =
      val lvl = summon[ValueOf[Lvl]].value
      s"Lvl {" + lvl + "}"

  given stateIntrospect
    [Name <: String & Singleton, I <: Int & Singleton]
    (using ValueOf[Name], ValueOf[I]):
    Eval[(Introspect,
         Proxy[State[Name, I]]), String] with
    extension (t:
      (Introspect, Proxy[State[Name, I]])) def eval: String =
      val name = summon[ValueOf[Name]].value
      val idx = summon[ValueOf[I]].value
      s"" + name + "[" + idx + "]"




  given intNilListIntrospect:
    Eval[(Introspect, Proxy[IN]), String] with
    extension (t:
      (Introspect, Proxy[IN])) def eval: String =
      s""

  given intConsListIntrospect
    [I <: Int & Singleton,
    Rest <: IntList]
    (using
      ValueOf[I],
      Eval[(Introspect, Proxy[Rest]), String]):
    Eval[(Introspect, Proxy[IC[I, Rest]]), String] with
    extension (t:
      (Introspect, Proxy[IC[I, Rest]])) def eval: String =
      val i = summon[ValueOf[I]].value
      val restPair = (Introspect(), Proxy[Rest]())
      s"" + i + " " + restPair.eval



  given condIntrospect
    [S <: IState, Cnts <: IntList]
    (using
      Eval[(Introspect, Proxy[S]), String],
      Eval[(Introspect, Proxy[Cnts]), String]):
    Eval[(Introspect,
         Proxy[NeighborsCount[S, Cnts]]), String] with
    extension (t:
      (Introspect, Proxy[NeighborsCount[S, Cnts]])) def eval: String =
      val sPair = (Introspect(), Proxy[S]())
      val cntsPair = (Introspect(), Proxy[Cnts]())
      sPair.eval + ", " + cntsPair.eval


  given stateTransIntrospect
    [From <: IState,
    To <: IState,
    Conds <: ICellCondition]
    (using
      Eval[(Introspect, Proxy[From]), String],
      Eval[(Introspect, Proxy[To]), String],
      Eval[(Introspect, Proxy[Conds]), String]
    ):
    Eval[(Introspect,
         Proxy[StateTransition[From, To, Conds]]), String] with
    extension (t:
      (Introspect, Proxy[StateTransition[From, To, Conds]])) def eval: String =
      val fromPair  = (Introspect(), Proxy[From]())
      val toPair    = (Introspect(), Proxy[To]())
      val condsPair = (Introspect(), Proxy[Conds]())
      fromPair.eval + " -> "
        + toPair.eval + ": "
        + condsPair.eval


  given tsNilListIntrospect:
    Eval[(Introspect, Proxy[Nil[IStateTransition]]), String] with
    extension (t:
      (Introspect, Proxy[Nil[IStateTransition]])) def eval: String =
      s""

  given tsConsListIntrospect
    [ S <: IStateTransition
    , Rest <: HList[IStateTransition]]
    (using
      Eval[(Introspect, Proxy[S]), String],
      Eval[(Introspect, Proxy[Rest]), String]):
    Eval[(Introspect, Proxy[Cons[IStateTransition, S, Rest]]), String] with
    extension (t:
      (Introspect, Proxy[Cons[IStateTransition, S, Rest]])) def eval: String =
      val sPair = (Introspect(), Proxy[S]())
      val restPair = (Introspect(), Proxy[Rest]())
      s"\n      " + sPair.eval + restPair.eval

  given stepIntrospect
    [DefState <: IState,
    Transitions <: HList[IStateTransition]]
    (using
      Eval[(Introspect, Proxy[DefState]), String],
      Eval[(Introspect, Proxy[Transitions]), String]
    ):
    Eval[(Introspect,
         Proxy[Step[DefState, Transitions]]), String] with
    extension (t:
      (Introspect, Proxy[Step[DefState, Transitions]])) def eval: String =
        val statePair = (Introspect(), Proxy[DefState]())
        val tsPair    = (Introspect(), Proxy[Transitions]())
        s"\n  Default state: "
          + statePair.eval
          + "\n  State transitions:"
          + tsPair.eval

  given ruleIntrospect
    [Name <: String & Singleton,
    Code <: String & Singleton,
    Neighborhood <: INeighborhood,
    Step <: IStep]
    (using ValueOf[Name],
           ValueOf[Code],
           Eval[(Introspect, Proxy[Neighborhood]), String],
           Eval[(Introspect, Proxy[Step]), String]
    ):
    Eval[(Introspect,
         Proxy[Rule[Name, Code, Neighborhood, Step]]),
         String] with
    extension (t:
      (Introspect,
      Proxy[Rule[Name, Code, Neighborhood, Step]])) def eval: String =

      val nhPair   = (Introspect(), Proxy[Neighborhood]())
      val stepPair = (Introspect(), Proxy[Step]())

      val name = summon[ValueOf[Name]].value
      val code = summon[ValueOf[Code]].value
      s"Rule <" + name + "> [" + code
        + "]\n  Neighborhood: "
        + nhPair.eval
        + stepPair.eval


}




