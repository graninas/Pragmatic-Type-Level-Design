use std::marker::PhantomData;

mod evidence_macro;
mod adts;

use adts::bool::Bool;
use adts::bool::True;
use adts::bool::False;

use adts::user::Teacher;
use adts::user::Student;
use adts::user::PowerUser;
use adts::user::RegularUser;

// User-defined bool types

type Enabled = Bool<True>;
type Disabled = Bool<False>;

#[allow(dead_code)]
type Invalid = Bool<i32>;       // will compile unless used



// User-defined domain types

type TeacherPowerUser = PowerUser<Teacher, true>;
type StudentUser = RegularUser<Student, true>;

fn main() {

  let _enabled: Enabled = Bool::<True> { _marker: PhantomData::<True> };

  let _disabled: Disabled = Bool::<False> { _marker: PhantomData::<False> };

  // won't compile
  // let invalid: Invalid = Bool::<i32> { _marker: PhantomData::<i32> };


  let _teacher_power_user: TeacherPowerUser =
    PowerUser::<Teacher, true> { _marker: PhantomData::<Teacher> };

  let _student_user: StudentUser =
    RegularUser::<Student, true> { _marker: PhantomData::<Student> };
}
