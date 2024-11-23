package graninas
package typelevel


type IsSubtype[A, B] = A match
  case B => true
  case _ => false


// Function for asserting on type equality
def ensureEqualTypes[A, B](using ev: A =:= B): Unit = ()
