import scala.compiletime._

import cellular.typelevel._
import cellular.language._
import cellular.extensions._
import cellular.assets._

import cellular.introspection._
import cellular.introspection.Introspection.{*, given}

type Rules =
  Cons[IRule, GoL.GoLRule,
    Cons[IRule, Seeds.SeedsRule,
      Nil[IRule]
    ]
  ]


@main def hello(): Unit =
  val golProxy = Proxy[GoL.GoLRule]()
  val seedsProxy = Proxy[Seeds.SeedsRule]()
  val rulesProxy = Proxy[Rules]()

  // println(ruleDescr.describe(golProxy))
  // println(ruleDescr.describe(seedsProxy))
  println(rulesProxy.describe)
