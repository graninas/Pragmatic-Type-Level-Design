package graninas
package typelevel



sealed trait HList
case class Nil() extends HList
case class Cons[T, Tail <: HList]() extends HList

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





type Append[T, Tail <: HList] = Cons[T, Tail]

type Head[L <: HList] = L match
  case Nil => Nothing
  case Cons[v, _] => v

type Tail[L <: HList] = L match
  case Nil => Nil
  case Cons[_, t] => t

type Lookup[Key, L <: HList] = L match
  case Nil => Nothing
  case Cons[i, t] => i match
    case (Key, v) => v
    case _ => Lookup[Key, t]
  case Cons[_, t] => Lookup[Key, t]


// TODO: rest of the funtions



// Function for asserting on type equality
def ensureEqualTypes[A, B](using ev: A =:= B): Unit = ()
