use std::marker::PhantomData;
use type_level::IInterface;
use type_level::Wrapper;
use tl_list_lib::HList;
use tl_str_list::TlStr;


pub struct IRoute;
pub struct IMethod;
pub struct IClause;
pub struct IType;
pub struct IFormat;

pub type PathString = String;


pub struct TypeImpl<
    Name: TlStr
  >
  (PhantomData::<Name>);
pub type Type<Name> = Wrapper<IType, TypeImpl<Name>>;


pub struct CustomTypeImpl<T>
  (PhantomData::<T>);
pub type CustomType<T> = Wrapper<IType, CustomTypeImpl<T>>;


pub struct FormatImpl<
    Name: TlStr
  >
  (PhantomData::<Name>);

pub type Format<Name> =
  Wrapper<IFormat, FormatImpl<Name>>;



pub struct QueryParamImpl<
    Name: TlStr,
    Type: IInterface<IType>
  >
  (PhantomData::<(Name, Type)>);

pub type QueryParam<Name, Type> =
  Wrapper<IClause, QueryParamImpl<Name, Type>>;


pub struct CaptureImpl<
    Name: TlStr,
    Type: IInterface<IType>
  >
  (PhantomData::<(Name, Type)>);

pub type Capture<Name, Type> =
  Wrapper<IClause, CaptureImpl<Name, Type>>;

pub struct PostMethodImpl;
pub struct GetMethodImpl;

pub type POST = Wrapper<IMethod, PostMethodImpl>;
pub type GET = Wrapper<IMethod, GetMethodImpl>;

pub struct RouteImpl<
    Method: IInterface<IMethod>,
    Path: TlStr,
    Clauses: HList<IClause>,
    SupportedFormats: HList<IFormat>,
    ReturnType: IInterface<IType>
  >
  (PhantomData::<(Method, Path, Clauses, SupportedFormats, ReturnType)>);

pub type Route<Method, Path, Clauses, Formats, ReturnType> =
  Wrapper<IRoute, RouteImpl<Method, Path, Clauses, Formats, ReturnType>>;


pub struct Api<
    Routes: HList<IRoute>
  >
  (PhantomData::<Routes>);
