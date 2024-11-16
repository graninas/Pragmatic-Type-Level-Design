package cellular
package introspection

import cellular.typelevel._
import cellular.language._
import cellular.extensions._

object Introspection {

  trait Description[T]:
    extension (t: T) def describe: String

  given nilDescr [K]:
    Description[Proxy[Nil[K]]] with
    extension (t: Proxy[Nil[K]]) def describe: String = s""

  given consDescr [K, T <: K, Rest <: HList[K]]
    (using
      Description[Proxy[Rest]],
      Description[Proxy[T]]):
    Description[Proxy[Cons[K, T, Rest]]] with
    extension (t: Proxy[Cons[K, T, Rest]]) def describe: String =
      val tProxy = Proxy[T]()
      val restProxy = Proxy[Rest]()
      s"${tProxy.describe}\n${restProxy.describe}"


  given ruleDescr
    [Name <: String & Singleton,
    Code <: String & Singleton,
    Neighborhood <: INeighborhood,
    Step <: IStep]:
    Description[Proxy[Rule[Name, Code, Neighborhood, Step]]] with
    extension (t: Proxy[Rule[Name, Code, Neighborhood, Step]]) def describe: String =
      s"Rule<> []\n  Neighborhood: "

}
