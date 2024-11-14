
trait Description[T]:
  extension (t: T) def describe: String


case class Proxy[T]()


sealed trait HList
case class Nil() extends HList
case class Cons[T, Tail]() extends HList

given nilDescr:
  Description[Proxy[Nil]] with
  extension (t: Proxy[Nil]) def describe: String = s"[]"

given consDescr [T, Rest <: HList]
  (using
    Description[Proxy[Rest]],
    Description[Proxy[T]]):
  Description[Proxy[Cons[T, Rest]]] with
  extension (t: Proxy[Cons[T, Rest]]) def describe: String =
    val tProxy = Proxy[T]()
    val restProxy = Proxy[Rest]()
    s"${tProxy.describe} : ${restProxy.describe}"


// User-defined types

case class Shakespeare()
case class Byron()
case class Pushkin()

type Poets = Cons[Shakespeare, Cons[Byron, Cons[Pushkin, Nil]]]

given shDescr: Description[Proxy[Shakespeare]] with
  extension (t: Proxy[Shakespeare]) def describe: String =
    s"Shakespeare"

given byDescr: Description[Proxy[Byron]] with
  extension (t: Proxy[Byron]) def describe: String =
    s"Byron"

given puDescr: Description[Proxy[Pushkin]] with
  extension (t: Proxy[Pushkin]) def describe: String =
    s"Pushkin"


type ::[H, T <: HList] = Cons[H, T]

type Poets2 = Shakespeare :: Byron :: Pushkin :: Nil

@main def run(): Unit =
  val poets = Proxy[Poets]()
  val poets2 = Proxy[Poets2]()
  println(poets.describe)
  println(poets2.describe)
