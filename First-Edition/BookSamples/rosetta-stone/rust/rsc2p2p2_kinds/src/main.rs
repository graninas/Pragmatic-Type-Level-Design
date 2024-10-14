use std::marker::PhantomData;

mod adts;

use adts::bool_kind::True;
use adts::bool_kind::False;

use adts::user::Teacher;
use adts::user::Student;
use adts::user::PowerUser;
use adts::user::RegularUser;


// User-defined domain types

type TeacherPowerUser = PowerUser<Teacher, True>;
type StudentUser = RegularUser<Student, False>;

#[allow(dead_code)]
const TEACHER_EVIDENCE: PhantomData<TeacherPowerUser> = PhantomData;
#[allow(dead_code)]
const STUDENT_EVIDENCE: PhantomData<StudentUser> = PhantomData;

#[allow(dead_code)]
type InvalidUser = RegularUser<i32, bool>;

// Won't compile:
// #[allow(dead_code)]
// const INVALID_EVIDENCE: PhantomData<InvalidUser> = PhantomData;

fn main() {

  let _teacher_power_user: TeacherPowerUser =
    PowerUser::<Teacher, True> (PhantomData::<(Teacher, True)>);

  let _student_user: PhantomData<StudentUser> = PhantomData;
}
