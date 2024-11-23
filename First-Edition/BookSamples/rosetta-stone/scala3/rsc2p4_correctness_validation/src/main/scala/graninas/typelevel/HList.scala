package graninas
package typelevel

sealed trait HList[Kind]
case class Nil[Kind]() extends HList[Kind]
case class Cons[Kind, T <: Kind, Tail <: HList[Kind]]() extends HList[Kind]

type HL[Kind] = Nil[Kind]


// Weird!! Operators ending with colon are right-associative,
// and others are left-associative!!!
type :+[L, V] = CheckAppend[V, L]
type +:[V, L] = CheckAppend[V, L]

type CheckAppend[V, L] = L match
  case Nil[k] => IsSubtype[V, k] match
    case true  => Cons[k, V, Nil[k]]
    case false => "kind mismatch"
  case Cons[k, v, t] => IsSubtype[V, k] match
    case true  => Cons[k, V, Cons[k, v, t]]
    case false => "kind mismatch"
  case _ => "arguments type mismatch"



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

