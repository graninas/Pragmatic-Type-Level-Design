import scala.compiletime._

import graninas.typelevel._
import cellular.language._
import cellular.extensions._
import cellular.assets._

import cellular.introspection._
import cellular.introspection.Introspection.{*, given}


@main def hello(): Unit =
  val pair = (Introspect(), Proxy[GoL.GoLRule]())
  println(pair.eval)

