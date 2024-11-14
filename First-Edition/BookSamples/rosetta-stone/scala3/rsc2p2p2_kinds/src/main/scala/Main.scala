trait Description[T]:
  extension (t: T) def describe: String

sealed trait BoolKind
case class True() extends BoolKind
case class False() extends BoolKind

sealed trait PersonKind
case class Teacher() extends PersonKind
case class Student() extends PersonKind

sealed trait UserKind[P <: PersonKind, V <: BoolKind]
case class PowerUser[P <: PersonKind, V <: BoolKind]() extends UserKind[P, V]
case class RegularUser[P <: PersonKind, V <: BoolKind]() extends UserKind[P, V]

case class Proxy[T]()

given tDescr:
  Description[Proxy[Teacher]] with
  extension (t: Proxy[Teacher]) def describe: String =
    s"Teacher"

given sDescr:
  Description[Proxy[Student]] with
  extension (t: Proxy[Student]) def describe: String =
    s"Student"

given trueDescr:
  Description[Proxy[True]] with
  extension (t: Proxy[True]) def describe: String =
    s"True"

given falseDescr:
  Description[Proxy[False]] with
  extension (t: Proxy[False]) def describe: String =
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


type TeacherPowerUser = PowerUser[Teacher, True]
type StudentUser = RegularUser[Student, False]

// Won't compile: types mismatch
// type InvalidUser = PowerUser[Teacher, Teacher]

@main def hello(): Unit =
  val teacher = Proxy[TeacherPowerUser]()
  val student = Proxy[StudentUser]()
  println(teacher.describe)
  println(student.describe)
