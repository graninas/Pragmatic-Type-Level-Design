import scala.compiletime._

import graninas.typelevel._
import cellular.language._
import cellular.extensions._
import cellular.assets._

import cellular.introspection._
import cellular.introspection.Introspection.{*, given}
import cellular.integrity.Integrity.{*, given}


type States = Cons[IState, GoL.D, Cons[IState, GoL.A, Nil[IState]]]
type Verified = VerifyStateInList[GoL.A, Cons[IState, GoL.D, Nil[IState]]]

val eqSts = ensureEqualTypes[GoL.States, States]
// val verified = ensureEqualTypes[false, IsSubtype[GoL.A, GoL.D]]
// val result: Nothing = Proxy[VerifyStateInList[GoL.A, States]]()


type IsEq[A <: IState, B <: IState] = (A, B) match
  case (MkState[aImpl], MkState[bImpl]) => IsSubtype[aImpl, bImpl]
  case _ => false

type A = State["A", 0]
type D = State["D", 1]

type A_Eq_A = IsEq[A, A]
type A_NotEq_D = IsEq[A, D]
val a_eq_a_evidence: A_Eq_A = true
val a_noteq_b_evidence: A_NotEq_D = false


@main def hello(): Unit =


  // val sts: Proxy[HList[IState]] = Proxy[GoL.States]()

  val statePair = (Introspect(), Proxy[GoL.A]())
  println(statePair.eval)

  val pair = (Introspect(), Proxy[GoL.GoLRule]())
  println(pair.eval)

