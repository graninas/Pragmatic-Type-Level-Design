object StaticDescriptionDemo:

  // Simulating an empty ADT with sealed traits
  sealed trait EmptyADT1
  sealed trait EmptyADT2

  trait StaticDescription[T]:
    def staticDescribe: String

  given EmptyADT1StaticDesc: StaticDescription[EmptyADT1] with
    def staticDescribe: String = "EmptyADT1"

  given EmptyADT2StaticDesc: StaticDescription[EmptyADT2] with
    def staticDescribe: String = "EmptyADT2"

  // Helper method for static dispatch
  def describe[T](using desc: StaticDescription[T]): String = desc.staticDescribe

  @main def run(): Unit =
    println(describe[EmptyADT1])
    println(describe[EmptyADT2])
