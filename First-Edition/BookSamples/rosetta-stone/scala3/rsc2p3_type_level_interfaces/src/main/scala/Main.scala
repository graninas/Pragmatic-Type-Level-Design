import scala.compiletime._

import graninas.typelevel._
import cellular.language._
import cellular.extensions._
import cellular.assets._

import cellular.introspection._
import cellular.introspection.Introspection.{*, given}

// Testing various combinations of types and type-level lists

sealed trait IVal extends IInterface
type MkVal[A] = IVal {
  type Impl = A
}

case class ValImpl[Name <: String & Singleton]()
type Val[Name <: String & Singleton] = MkVal[ValImpl[Name]]

sealed trait IObj extends IInterface
type MkObj[A] = IObj {
  type Impl = A
}

case class ObjImpl[Name <: String & Singleton]()
type Obj[Name <: String & Singleton] = MkObj[ObjImpl[Name]]

type V1 = Val["V1"]
type V2 = Val["V2"]
type O1 = Obj["O1"]
type O2 = Obj["O2"]

// Correct: val + []::[IVal] = [val]::[IVal]
type Test1 = CheckAppend[V1, Nil[IVal]]
val test1 = summon[Test1 =:= Cons[IVal, V1, Nil[IVal]]]

// Correct: val1 + [val2]::[IVal] = [val1, val2]::[IVal]
type Test2 = CheckAppend[V1, Cons[IVal, V2, Nil[IVal]]]
val test2 = summon[Test2 =:= Cons[IVal, V1, Cons[IVal, V2, Nil[IVal]]]]

// Incorrect: val + []::[IObj] = "kind mismatch"
type Test3 = CheckAppend[V1, Nil[IObj]]
val test3 = summon[Test3 =:= "kind mismatch"]

// Incorrect: val + [obj]::[IObj] = "kind mismatch"
type Test4 = CheckAppend[V1, Cons[IObj, O1, Nil[IObj]]]
val test4 = summon[Test4 =:= "kind mismatch"]

// Incorrect: val + obj = "arguments type mismatch"
type Test5 = CheckAppend[V1, O1]
val test5 = summon[Test5 =:= "arguments type mismatch"]


// Extension - some extra state type

case class RgbImpl[
  R <: Int & Singleton,
  G <: Int & Singleton,
  B <: Int & Singleton]()

type RGB[
  R <: Int & Singleton,
  G <: Int & Singleton,
  B <: Int & Singleton] = MkState[RgbImpl[R, G, B]]

type Red   = RGB[255, 0, 0]
type Green = RGB[0, 255, 0]
type Blue  = RGB[0, 0, 255]


@main def hello(): Unit =

  val statePair = (Introspect(), Proxy[GoL.A]())
  println(statePair.eval)

  val pair = (Introspect(), Proxy[GoL.GoLRule]())
  println(pair.eval)

