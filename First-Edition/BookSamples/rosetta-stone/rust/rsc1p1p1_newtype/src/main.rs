
#[allow(dead_code)]
struct FirstName(&'static str);
#[allow(dead_code)]
struct LastName(&'static str);

#[allow(dead_code)]
const PERSON_FIRST_NAME: FirstName = FirstName("Jack");
const PERSON_LAST_NAME: LastName = LastName("O'Neill");

#[allow(dead_code)]
struct FilePath(&'static str);
const LAST_FILE_NAME: FilePath = FilePath("last_file_name.txt");

#[allow(unused_variables)]
fn write_last_name(path: FilePath, name: LastName) {
  // do something
}

fn main() {
  write_last_name(LAST_FILE_NAME, PERSON_LAST_NAME);

  // Won't compile:
  // write_last_name(LAST_FILE_NAME, PERSON_FIRST_NAME);
  // expected `LastName`, found `FirstName`
}
