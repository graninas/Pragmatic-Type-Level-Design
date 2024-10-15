use std::marker::PhantomData;

struct Nil;
struct Cons<Head, Tail>(PhantomData<(Head, Tail)>);

// Append operation

trait AppendImpl<N> {
  type Output;
}

impl<N> AppendImpl<N> for Nil
{
  type Output = Cons<N, Nil>;
}

impl<N, M, List> AppendImpl<N> for Cons<M, List>
{
  type Output = Cons<N, Cons<M, List>>;
}

// Head operation

trait HeadImpl {
  type Output;
}

impl<N, List> HeadImpl for Cons<N, List> {
  type Output = N;
}

// Tail operation

trait TailImpl {
  type Output;
}

impl TailImpl for Nil {
  type Output = Nil;
}

impl<N, Rest> TailImpl for Cons<N, Rest> {
  type Output = Rest;
}

// Interface
type Append<N, List> = <List as AppendImpl<N>>::Output;
type Head<List> = <List as HeadImpl>::Output;
type Tail<List> = <List as TailImpl>::Output;



// Description

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
  type Poets = Cons<Byron, Cons<Pushkin, Nil>>;

  println!("{}", Poets::describe());
  // > Lord Byron : Alexander Pushkin : []

  type Poets2 = Append<Shakespeare, Poets>;
  println!("{}", Poets2::describe());
  // > William Shakespeare : Lord Byron : Alexander Pushkin : []

  type SomePoet = Head<Poets>;
  println!("{}", SomePoet::describe());
  // > Lord Byron

  // Will not compile (no such trait)
  // type SomePoet2 = Head<Nil>;
  // println!("{}", SomePoet2::describe());

  type Poets3 = Tail<Poets>;
  println!("{}", Poets3::describe());
  // > Alexander Pushkin : []
}



