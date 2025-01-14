use type_level::IInterface;
use tl_list_lib::HList;
use std::marker::PhantomData;

// -- Interfaces

pub struct IRoute;

pub struct RouteWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IRoute> for RouteWrapper<T> {
  type Interface = IRoute;
}

pub struct IClause;

pub struct ClauseWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IClause> for ClauseWrapper<T> {
  type Interface = IClause;
}


pub struct IType;

pub struct TypeWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IType> for TypeWrapper<T> {
  type Interface = IType;
}





// TODO: maybe add hierarchy (IQueryParam is IClause)?

pub struct IQueryParam;

pub struct QueryParamWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IQueryParam> for QueryParamWrapper<T> {
  type Interface = IQueryParam;
}

