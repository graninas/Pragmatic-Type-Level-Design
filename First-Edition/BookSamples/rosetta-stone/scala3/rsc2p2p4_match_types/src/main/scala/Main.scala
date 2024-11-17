import graninas.typelevel.{*, given}

import scala.reflect.TypeTest

type List1 = Cons[1, Nil]
type List1Updated = Append[2, List1]
type List2 = Cons[2, Cons[1, Nil]]
type Val1 = Head[List2]


type AccocList = Cons[(1, "a"), Cons[(2, "b"), Nil]]
type Val = Lookup[2, AccocList]


val list1EqList2 = ensureEqualTypes[List1Updated, List2]
val list1EqList3 = ensureEqualTypes[Val1, 2]

val valEqB = ensureEqualTypes[Val1, 2]

@main def hello(): Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
