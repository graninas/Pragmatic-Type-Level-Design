package cellular
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
