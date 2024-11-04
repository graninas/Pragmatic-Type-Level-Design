
class FirstName(protected val name: String):
  override def toString(): String = name

object FirstName:
  def apply(name: String): FirstName = new FirstName(name)

class LastName(protected val name: String):
  override def toString(): String = name

object LastName:
  def apply(name: String): LastName = new LastName(name)



class FilePath(protected val path: String):
  override def toString(): String = path

object FilePath:
  def apply(path: String): FilePath = new FilePath(path)



def writeLastName(path: FilePath, name: LastName): Unit =
  println("File is written!");

@main
def hello(): Unit =
  val PersonFirstName: FirstName = FirstName("Jack");
  val PersonLastName: LastName = LastName("O'Neill");
  val LastFileName: FilePath = FilePath("last_file_name.txt");

  writeLastName(LastFileName, PersonLastName);
  // Won't compile, types mismatch:
  // writeLastName(LastFileName, PersonFirstName);
