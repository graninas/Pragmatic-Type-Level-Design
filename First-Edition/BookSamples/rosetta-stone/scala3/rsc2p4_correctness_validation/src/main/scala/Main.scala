import scala.compiletime._

import graninas.typelevel._
import cellular.language._
import cellular.extensions._
import cellular.assets._

import cellular.introspection._
import cellular.introspection.Introspection.{*, given}
import cellular.integrity.Integrity.{*, given}


val stateIsValid: CheckStateInList[GoL.A, GoL.States] = true

type Test1 = CheckStateInList[GoL.A, GoL.States]
val test1 = ensureEqualTypes[Test1, true]

type Test2 = CheckStateInList[GoL.D, GoL.States]
val test2 = ensureEqualTypes[Test2, true]

type Test2V = StateInList[GoL.D, GoL.States]
val test2V = Proxy[Test2V]().verify

type Test3 = CheckStateInList[GoL.Unknown, GoL.States]
val test3 = ensureEqualTypes[Test3, false]

// Won't compile. Error massage is okay
// type Test3Verify = StateInList[GoL.Unknown, GoL.States]
// val test3Verify = Proxy[Test3Verify]().verify

// Won't compile. Error message is okay:
// Type argument (4 : Int) does not conform to upper bound IState
// type Test4 = CheckStateInList[4, GoL.States]

type Test5 = CheckStateInList[GoL.A, Nil[IState]]
val test5 = ensureEqualTypes[Test5, false]

// Won't compile. Error massage is okay
// type Test5Verify = StateInList[GoL.A, Nil[IState]]
// val test5Verify = Proxy[Test5Verify]().verify


type Test6 = ValidateDefaultState[GoL.GoLStep]
val test6 = ensureEqualTypes[Test6, true]


val test: ValidateDefaultState[GoL.GoLStep] = true


@main def hello(): Unit =

  val statePair = (Introspect(), Proxy[GoL.A]())
  println(statePair.eval)

  val withIntegrity = WithIntegrity[Introspect, GoL.States]()
  val pair = (withIntegrity, Proxy[GoL.GoLRule]())
  println(pair.eval)

