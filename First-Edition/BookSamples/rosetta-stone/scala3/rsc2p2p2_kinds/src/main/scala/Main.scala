trait Description[T]:
  extension (t: T) def describe: String



sealed trait BoolKind
case object True extends BoolKind
case object False extends BoolKind

sealed trait PersonKind
case object Teacher extends PersonKind
case object Student extends PersonKind

sealed trait UserKind[P <: PersonKind, V <: BoolKind]

case class PowerUser[P <: PersonKind, V <: BoolKind]() extends UserKind[P, V]
case class RegularUser[P <: PersonKind, V <: BoolKind]() extends UserKind[P, V]

case class Proxy[T]()

given tDescr:
  Description[Proxy[Teacher.type]] with
  extension (t: Proxy[Teacher.type]) def describe: String =
    s"Teacher"

given sDescr:
  Description[Proxy[Student.type]] with
  extension (t: Proxy[Student.type]) def describe: String =
    s"Student"

given trueDescr:
  Description[Proxy[True.type]] with
  extension (t: Proxy[True.type]) def describe: String =
    s"True"

given falseDescr:
  Description[Proxy[False.type]] with
  extension (t: Proxy[False.type]) def describe: String =
    s"False"

given puDescr [P <: PersonKind, V <: BoolKind]
  (using Description[Proxy[P]],
  Description[Proxy[V]]):
  Description[Proxy[PowerUser[P, V]]] with
  extension (t: Proxy[PowerUser[P, V]]) def describe: String =
    val pProxy = Proxy[P]()
    val vProxy = Proxy[V]()
    s"PowerUser(${pProxy.describe}, valid: ${vProxy.describe})"

given ruDescr [P <: PersonKind, V <: BoolKind]
  (using Description[Proxy[P]],
  Description[Proxy[V]]):
  Description[Proxy[RegularUser[P, V]]] with
  extension (t: Proxy[RegularUser[P, V]]) def describe: String =
    val pProxy = Proxy[P]()
    val vProxy = Proxy[V]()
    s"RegularUser(${pProxy.describe}, valid: ${vProxy.describe})"


type TeacherPowerUser = PowerUser[Teacher.type, True.type]
type StudentUser = RegularUser[Student.type, False.type]


@main def hello(): Unit =
  val teacher = Proxy[TeacherPowerUser]()
  val student = Proxy[StudentUser]()
  println(teacher.describe)
  println(student.describe)
