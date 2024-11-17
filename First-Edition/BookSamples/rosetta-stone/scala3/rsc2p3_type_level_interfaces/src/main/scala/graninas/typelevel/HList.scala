package graninas
package typelevel

sealed trait HList[Kind]
case class Nil[Kind]() extends HList[Kind]
case class Cons[Kind, T <: Kind, Tail <: HList[Kind]]() extends HList[Kind]

// TODO: a better UX for lists


sealed trait IntList
case class IN() extends IntList
case class IC[I <: Int & Singleton, Tail <: IntList]() extends IntList

type ::[T <: Int & Singleton, Tail <: IntList] = IC[T, Tail]



// Proposed by Mateusz Kubuszok:
//   type IsSubtype[A, B] = A match
//     case B => true
//     case _ => false

//   type TLList[Kind, T <: Tuple] = T match
//     case EmptyTuple => Nil[Kind]
//     case h *: t =>
//       IsSubtype[h, Kind] match
//         case true  => Cons[Kind, h, TLList[Kind, t]]
//         case false => "nope"

// this yields this syntax:

// type Shorter = TLList[IState, (A, D)]





type Append[K, T <: K, Tail <: HList[K]] = Cons[K, T, Tail]

type Head[T] = T match
  case Nil[_] => Nothing
  case Cons[_, v, _] => v

type Tail[L] = L match
  case Nil[s] => Nil[s]
  case Cons[_, _, t] => t

type Lookup[Key, L] = L match
  case Nil[_] => Nothing
  case Cons[_, Key *: v, t] => v
  case Cons[_, _, t] => Lookup[Key, t]


// TODO: rest of the funtions




sealed trait HList
case class Nil() extends HList
case class Cons[T :< Tuple, Tail <: HList]() extends HList



// Function for asserting on type equality
def ensureEqualTypes[A, B](using ev: A =:= B): Unit = ()
