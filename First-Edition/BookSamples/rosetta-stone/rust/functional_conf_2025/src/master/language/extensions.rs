use crate::master::language::web::{*};
use crate::master::language::model::{*};

use std::marker::PhantomData;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_list_lib::HList;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;
use tl_list_lib::tl_list;
use tl_list_lib::tl_i32_list;


type IntType = Type<tl_str!("int")>;
type StringType = Type<tl_str!("string")>;
