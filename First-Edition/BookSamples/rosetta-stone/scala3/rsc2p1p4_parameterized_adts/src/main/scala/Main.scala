trait Description[T]:
  extension (t: T) def describe: String

// Empty ADTs

case object EmptyADT
case object EmptyADT2

given eADTDescr: Description[EmptyADT.type] with
  extension (t: EmptyADT.type) def describe: String = "EmptyADT"

given eADT2Descr: Description[EmptyADT2.type] with
  extension (t: EmptyADT2.type) def describe: String = "EmptyADT2"


// Parameterized ADTs

case class Proxy[T]()

case object Benoit
case class Mandelbrot[T]()
// if we need a parameterized ADT, we have to use
// case class. But we don't need a field, which is mandatory; so we have to use Proxy.

type Fractal = Mandelbrot[Mandelbrot[Benoit.type]]

given mandelDescr [T] (using Description[Proxy[T]]):
  Description[Proxy[Mandelbrot[T]]] with
  extension (t: Proxy[Mandelbrot[T]]) def describe: String =
    val proxy = Proxy[T]()
    s"Mandelbrot(${proxy.describe})"

given benoitDescr [T]:
  Description[Proxy[Benoit.type]] with
  extension (t: Proxy[Benoit.type]) def describe: String =
    s"Benoit"


@main def run(): Unit =
  val eADT = EmptyADT
  val eADT2 = EmptyADT2
  println(eADT.describe)
  println(eADT2.describe)

  val fractal = Proxy[Fractal]()
  println(fractal.describe)

