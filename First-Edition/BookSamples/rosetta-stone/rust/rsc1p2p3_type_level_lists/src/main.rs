use std::marker::PhantomData;

struct Nil;
struct Cons<Head, Tail>(PhantomData<(Head, Tail)>);

trait Description {
  fn describe() -> String;
}

impl Description for Nil {
  fn describe() -> String {
    "[]".to_string()
  }
}

impl<Head, Tail> Description for Cons<Head, Tail>
  where
    Head: Description,
    Tail: Description,
{
  fn describe() -> String {
    format!("{} : {}", Head::describe(), Tail::describe())
  }
}

// User-defined types

struct Shakespeare;
struct Byron;
struct Pushkin;

impl Description for Shakespeare {
  fn describe() -> String {
    "William Shakespeare".to_string()
  }
}

impl Description for Byron {
  fn describe() -> String {
    "Lord Byron".to_string()
  }
}

impl Description for Pushkin {
  fn describe() -> String {
    "Alexander Pushkin".to_string()
  }
}

fn main() {
  type Poets = Cons<Shakespeare, Cons<Byron, Cons<Pushkin, Nil>>>;

  println!("{}", Poets::describe());
}

