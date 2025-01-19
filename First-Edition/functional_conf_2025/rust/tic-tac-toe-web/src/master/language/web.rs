use type_level::IInterface;
use tl_list_lib::HList;
use std::marker::PhantomData;

// -- Interfaces

pub struct IRoute;

pub struct RouteWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IRoute> for RouteWrapper<T> {
  type Interface = IRoute;
}

pub struct IMethod;

pub struct MethodWrapper<T> (PhantomData::<T>);
impl<T> IInterface<IMethod> for MethodWrapper<T> {
  type Interface = IMethod;
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


pub struct ISupportedFormat;

pub struct SupportedFormatWrapper<T> (PhantomData::<T>);
impl<T> IInterface<ISupportedFormat> for SupportedFormatWrapper<T> {
  type Interface = ISupportedFormat;
}





// TODO: maybe add hierarchy (IQueryParam is IClause)?


// pub struct IQueryParam;
// pub struct QueryParamWrapper<T> (PhantomData::<T>);
// impl<T> IInterface<IQueryParam> for QueryParamWrapper<T> {
//   type Interface = IQueryParam;
// }



pub type PathString = String;
