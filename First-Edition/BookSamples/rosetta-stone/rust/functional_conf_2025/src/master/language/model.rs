use crate::master::language::web::{*};

use std::marker::PhantomData;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_list_lib::HList;
use tl_str_list::TlStr;


pub struct RouteImpl<
  R: TlStr,
  Clauses: HList<IClause>>
  (PhantomData::<(R, Clauses)>);

pub type Route<R, Clauses> = RouteWrapper<RouteImpl<R, Clauses>>;



pub struct QueryParamImpl<
  Name: TlStr,
  Type: IInterface<IType>>
  (PhantomData::<(Name, Type)>);

pub type QueryParam<Name, Type> =
  QueryParamWrapper<QueryParamImpl<Name, Type>>;



pub struct TypeImpl<
  Name: TlStr>
  (PhantomData::<Name>);

pub type Type<Name> = TypeWrapper<TypeImpl<Name>>;
