use crate::master::language::web::{*};

use std::marker::PhantomData;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_list_lib::HList;
use tl_str_list::TlStr;


pub struct RouteImpl<
  Path: TlStr,
  Method: IInterface<IMethod>,
  Clauses: HList<IClause>>
  (PhantomData::<(Path, Method, Clauses)>);

pub type Route<Path, Method, Clauses> =
  RouteWrapper<RouteImpl<Path, Method, Clauses>>;



pub struct QueryParamImpl<
  Name: TlStr,
  Type: IInterface<IType>>
  (PhantomData::<(Name, Type)>);

pub type QueryParam<Name, Type> =
  ClauseWrapper<QueryParamImpl<Name, Type>>;


pub struct CaptureImpl<
  Name: TlStr,
  Type: IInterface<IType>>
  (PhantomData::<(Name, Type)>);

pub type Capture<Name, Type> =
  ClauseWrapper<CaptureImpl<Name, Type>>;


pub struct PostMethodImpl<
  SupportedFormats: HList<ISupportedFormat>,
  ReturnType: IInterface<IType>>
  (PhantomData::<(SupportedFormats, ReturnType)>);

pub type PostMethod<Formats, ReturnType> =
  MethodWrapper<PostMethodImpl<Formats, ReturnType>>;

pub struct GetMethodImpl<
  SupportedFormats: HList<ISupportedFormat>,
  ReturnType: IInterface<IType>>
  (PhantomData::<(SupportedFormats, ReturnType)>);

pub type GetMethod<Formats, ReturnType> =
  MethodWrapper<GetMethodImpl<Formats, ReturnType>>;



pub struct TypeImpl<
  Name: TlStr>
  (PhantomData::<Name>);
pub type Type<Name> = TypeWrapper<TypeImpl<Name>>;


pub struct CustomTypeImpl<T>
  (PhantomData::<T>);
pub type CustomType<T> = TypeWrapper<CustomTypeImpl<T>>;


pub struct SupportedFormatImpl<
  Name: TlStr>
  (PhantomData::<Name>);

pub type SupportedFormat<Name> =
  SupportedFormatWrapper<SupportedFormatImpl<Name>>;


pub struct Api<
  Routes: HList<IRoute>>
  (PhantomData::<Routes>);
