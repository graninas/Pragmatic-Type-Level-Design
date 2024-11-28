package cellular
package integrity

import graninas.typelevel._
import cellular.language._
import cellular.extensions._

import scala.compiletime.*


object Integrity {

  case class WithIntegrity[Verb, Dict]()

  trait Verify[Tag]:
    extension (t: Tag) def verify: Boolean

  type CheckStateInList[S <: IState, L <: HList[IState]] =
    (S, L) match
      case (_, Nil[_]) => false
      case (MkState[s1Impl], Cons[_, s2, rest]) => s2 match
        case MkState[s2Impl] => IsSubtype[s1Impl, s2Impl] match
          case true => true
          case false => CheckStateInList[S, rest]


  // This works. Compilation fails for an empty list.
  // But the compiler error is absolutely irrelevant and unreadable.
  // It says: "value eval is not a member of...
  //           An extension method was tried, but could not be fully constructed"
  // The reason why compilation fails is not shown.
  //
  case class StateInList[S <: IState, L <: HList[IState]]()

  // // Instance should not be defined for empty list.
  // // given stInListNil
  // //   [S <: IState]:
  // //   Verify[StateInList[S, Nil[IState]]] with {}


  given stInListCons
    [S <: IState, V <: IState, L <: HList[IState]]
    (using CheckStateInList[S, L] =:= true):
    Verify[Proxy[StateInList[S, L]]] with
      extension (t: Proxy[StateInList[S, L]]) def verify: Boolean =
        true


  type ValidateRule[R <: IRule] = R match
    case MkRule[ruleImpl] => ruleImpl match
      case RuleImpl[_, _, _, step] =>
        ValidateDefaultState[step]

  type ValidateDefaultState[S <: IStep] = S match
    case MkStep[stepImpl] => stepImpl match
      case StepImpl[defState, ts] => defState match
        case MkState[stateImpl] =>
          CheckStateInTransitions[stateImpl, ts]

  type CheckStateInTransitions
    [StImpl, Ts <: HList[IStateTransition]] = Ts match
      case Nil[_] => false
      case Cons[_, trans, rest] => trans match
        case MkStateTransition[transImpl] => transImpl match
          case StateTransitionImpl[fromS, _, _] => fromS match
            case MkState[fromSImpl] =>
              IsSubtype[StImpl, fromSImpl] match
                case true => true
                case false => CheckStateInTransitions[StImpl, rest]






}




