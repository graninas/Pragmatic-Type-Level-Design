use crate::master::language::web::{*};

use std::marker::PhantomData;
use type_level::IInterface;
use tl_list_lib::I32List;
use tl_list_lib::HList;
use tl_str_list::TlStr;





pub struct TypeImpl<
    Name: TlStr
  >
  (PhantomData::<Name>);
pub type Type<Name> = TypeWrapper<TypeImpl<Name>>;


pub struct CustomTypeImpl<T>
  (PhantomData::<T>);
pub type CustomType<T> = TypeWrapper<CustomTypeImpl<T>>;


pub struct SupportedFormatImpl<
    Name: TlStr
  >
  (PhantomData::<Name>);

pub type SupportedFormat<Name> =
  SupportedFormatWrapper<SupportedFormatImpl<Name>>;



pub struct QueryParamImpl<
    Name: TlStr,
    Type: IInterface<IType>
  >
  (PhantomData::<(Name, Type)>);

pub type QueryParam<Name, Type> =
  ClauseWrapper<QueryParamImpl<Name, Type>>;


pub struct CaptureImpl<
    Name: TlStr,
    Type: IInterface<IType>
  >
  (PhantomData::<(Name, Type)>);

pub type Capture<Name, Type> =
  ClauseWrapper<CaptureImpl<Name, Type>>;

pub struct PostMethodImpl;
pub struct GetMethodImpl;

pub type POST = MethodWrapper<PostMethodImpl>;
pub type GET = MethodWrapper<GetMethodImpl>;

pub struct RouteImpl<
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    SupportedFormats: HList<ISupportedFormat>,
    ReturnType: IInterface<IType>
  >
  (PhantomData::<(Method, Path, Clauses, SupportedFormats, ReturnType)>);

pub type Route<Method, Path, Clauses, Formats, ReturnType> =
  RouteWrapper<RouteImpl<Method, Path, Clauses, Formats, ReturnType>>;


pub struct Api<
    Routes: HList<IRoute>
  >
  (PhantomData::<Routes>);
