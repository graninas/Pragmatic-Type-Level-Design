package graninas
package typelevel



trait Eval[T, Result]:
  extension (v: T) def eval: Result

