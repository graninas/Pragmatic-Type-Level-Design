package cellular
package integrity

import graninas.typelevel._
import cellular.language._
import cellular.extensions._

import scala.compiletime.*

object Integrity {

  trait Verify[Tag]

  case class StateInList[S <: IState, L <: HList[IState]]()

  // This works. Compilation fails for an empty list.
  // But the compiler error is absolutely irrelevant and unreadable.
  // It says: "value eval is not a member of...
  //           An extension method was tried, but could not be fully constructed"
  // The reason why compilation fails is not shown.
  // given stInListCons
  //   [S <: IState, V <: IState, Rest <: HList[IState]]:
  //   Verify[StateInList[S, Cons[IState, V, Rest]]] with {}

  // // Instance should not be defined for empty list.
  // // given stInListNil
  // //   [S <: IState]:
  // //   Verify[StateInList[S, Nil[IState]]] with {}
  // There should be a better way to do this.


  type VerifyStateInList[S <: IState, L <: HList[IState]] = L match
    case Nil[_] => false
    case (Cons[k, s, rest]) => IsSubtype[S, s]
    case _ => false

}




