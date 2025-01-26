use crate::master::language::model::{*};

use tl_str_macro::tl_str;


pub type IntType = Type<tl_str!("int")>;
pub type StringType = Type<tl_str!("string")>;
pub type DataType<T> = CustomType<T>;

pub type JSON = Format<tl_str!("JSON")>;
pub type PlainText = Format<tl_str!("PlainText")>;
pub type XML = Format<tl_str!("XML")>;


