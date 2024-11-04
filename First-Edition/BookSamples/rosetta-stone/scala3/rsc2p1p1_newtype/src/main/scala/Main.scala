
// #[allow(dead_code)]
// struct FirstName(&'static str);
// #[allow(dead_code)]
// struct LastName(&'static str);

// #[allow(dead_code)]
// const PERSON_FIRST_NAME: FirstName = FirstName("Jack");
// const PERSON_LAST_NAME: LastName = LastName("O'Neill");

// #[allow(dead_code)]
// struct FilePath(&'static str);
// const LAST_FILE_NAME: FilePath = FilePath("last_file_name.txt");

// #[allow(unused_variables)]
// fn write_last_name(path: FilePath, name: LastName) {
//   // do something
// }

// fn main() {
//   write_last_name(LAST_FILE_NAME, PERSON_LAST_NAME);
// }


// opaque type FirstName = String
// opaque type LastName = String

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
