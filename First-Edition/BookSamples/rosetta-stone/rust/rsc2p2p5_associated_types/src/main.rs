use std::marker::PhantomData;

use tl_list_lib::Nil;
use tl_list_lib::Cons;
use tl_list_lib::tl_list;
use tl_list_lib::Append;
use tl_list_lib::Tail;
use tl_list_lib::Eq;
use tl_list_lib::EqImpl;
use tl_list_lib::True;
use tl_list_lib::False;
use tl_list_lib::AppendImpl;

use assert_type_eq::assert_type_eq;

// User-defined types

enum A {}
enum B {}
enum C {}
enum D {}
enum X {}
enum Y {}

macro_rules! gen_eq_impl {
    ($T1:ty, $T2:ty, $TEq:ty) => {
        impl EqImpl<$T1> for $T2 {
            type A_ = $T1;
            type B_ = $T2;
            type Output = $TEq;
        }
    };
}

gen_eq_impl!(A, A, True);
gen_eq_impl!(B, B, True);
gen_eq_impl!(C, C, True);
gen_eq_impl!(D, D, True);
gen_eq_impl!(X, X, True);
gen_eq_impl!(Y, Y, True);
gen_eq_impl!(A, C, False);
gen_eq_impl!(A, D, False);
gen_eq_impl!(A, X, False);
gen_eq_impl!(A, Y, False);
gen_eq_impl!(B, A, False);
gen_eq_impl!(B, C, False);
gen_eq_impl!(B, D, False);
gen_eq_impl!(B, X, False);
gen_eq_impl!(B, Y, False);
gen_eq_impl!(C, A, False);
gen_eq_impl!(C, B, False);
gen_eq_impl!(C, D, False);
gen_eq_impl!(C, X, False);
gen_eq_impl!(C, Y, False);
gen_eq_impl!(D, A, False);
gen_eq_impl!(D, B, False);
gen_eq_impl!(D, C, False);
gen_eq_impl!(D, X, False);
gen_eq_impl!(D, Y, False);
gen_eq_impl!(X, A, False);
gen_eq_impl!(X, B, False);
gen_eq_impl!(X, C, False);
gen_eq_impl!(X, D, False);
gen_eq_impl!(X, Y, False);
gen_eq_impl!(Y, A, False);
gen_eq_impl!(Y, B, False);
gen_eq_impl!(Y, C, False);
gen_eq_impl!(Y, D, False);
gen_eq_impl!(Y, X, False);


#[allow(dead_code)]
type SrcList = tl_list![A, B, C];

#[allow(dead_code)]
type Appended = Append<X, SrcList>;

#[allow(dead_code)]
type AppendedExpected = tl_list![X, A, B, C];

#[allow(dead_code)]
type AppendedEq = Eq<Appended, AppendedExpected>;

#[allow(dead_code)]
type AppendedEqCheck = Eq<AppendedEq, True>;

#[allow(dead_code)]
const APPENDED_EQ_CHECK_EVIDENCE: PhantomData::<AppendedEqCheck> = PhantomData;


#[allow(dead_code)]
type Appended2 = <SrcList as AppendImpl<X>>::Output;

#[allow(dead_code)]
type Appended2EqCheck = Eq<Appended, Appended2>;

#[allow(dead_code)]
const APPENDED2_EVIDENCE: PhantomData::<Appended2EqCheck> = PhantomData;


type Tailed = Tail<SrcList>;
type TailedExpected = tl_list![B, C];
assert_type_eq!(Tailed, TailedExpected);


fn main() {


}



