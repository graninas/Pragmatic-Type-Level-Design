//! This module defines an extensible type-level
//! payment methods and payment processors definition eDSL.

use tl_list_lib::tl_list;
use tl_list_lib::HList;
use tl_list_lib::TlN_;
use tl_list_lib::TlC_;
use tl_interface::IInterface;
use tl_interface::Wrapper;
use tl_interface::EvalCtx;
use tl_interface::Eval;
use tl_str_list::TlStr;
use tl_str_macro::tl_str;

use std::marker::PhantomData;
use std::any::Any;
use std::collections::HashMap;

// Types infrastructure

pub struct ITypeTag;

pub struct StringTypeTag;
pub struct IntTypeTag;
pub struct DateTypeTag;
pub struct BoolTypeTag;
pub type StringType = Wrapper<ITypeTag, StringTypeTag>;
pub type IntType = Wrapper<ITypeTag, IntTypeTag>;
pub type DateType = Wrapper<ITypeTag, DateTypeTag>;
pub type BoolType = Wrapper<ITypeTag, BoolTypeTag>;

// Schema infrastructure

pub struct IField;
pub struct ISchema;

pub struct SchemaImpl<
  Fields: HList<IField>
  > (PhantomData<Fields>);

pub struct FieldImpl<
  Name: TlStr,
  TypeTag: IInterface<ITypeTag>,
  const MANDATORY: bool,
> (PhantomData<(Name, TypeTag)>);

pub struct SchemaFieldImpl<
  Name: TlStr,
  DataSchema: IInterface<ISchema>,
  const MANDATORY: bool,
> (PhantomData<(Name, DataSchema)>);

pub type Schema<Fields>
  = Wrapper<ISchema, SchemaImpl<Fields>>;
pub type Field<Name, TypeTag>
  = Wrapper<IField, FieldImpl<Name, TypeTag, true>>;
pub type SchemaField<Name, DataSchema>
  = Wrapper<IField, SchemaFieldImpl<Name, DataSchema, true>>;
pub type OptionalField<Name, TypeTag>
  = Wrapper<IField, FieldImpl<Name, TypeTag, false>>;
pub type OptionalSchemaField<Name, DataSchema>
  = Wrapper<IField, SchemaFieldImpl<Name, DataSchema, false>>;


// Payment methods

pub struct IPaymentMethodTag;
pub struct IPaymentMethod;

pub struct CurrencyTypeTag;
pub type CurrencyType = Wrapper<ITypeTag, CurrencyTypeTag>;

pub struct PaymentMethodImpl<
  // Tags substitute names and are used as references
  // to avoid nesting of types
  Tag: IInterface<IPaymentMethodTag>,
  DataSchema: IInterface<ISchema>,
  FundSource: HList<IPaymentMethodTag>,
  const REQUIRES_AUTH: bool,
> (PhantomData<(Tag, DataSchema, FundSource)>);

pub type PaymentMethod<
  Tag,
  DataSchema,
  FundSource,
  const REQUIRES_AUTH: bool,
> = Wrapper<IPaymentMethod,
  PaymentMethodImpl<Tag, DataSchema, FundSource, REQUIRES_AUTH>>;

// Currencies

pub struct ICurrency;

// Countries

pub struct ICountry;

// Authentication methods

pub struct IAuthMethod;

// Payment processors

pub struct IPaymentProcessorTag;
pub struct IPaymentProcessor;

// A payment feature is a specific combination
// of country, payment method, currency and autentication
pub struct IPaymentFeature;

pub struct PaymentFeatureImpl<
  Country: IInterface<ICountry>,
  Method: IInterface<IPaymentMethod>,
  Currency: IInterface<ICurrency>,
  AuthMethod: IInterface<IAuthMethod>,
> (PhantomData<(Country, Method, Currency, AuthMethod)>);

pub type PaymentFeature<Country, Method, Currency, AuthMethod>
  = Wrapper<IPaymentFeature, PaymentFeatureImpl<Country, Method, Currency, AuthMethod>>;

pub struct PaymentProcessorImpl<
  Tag: IInterface<IPaymentProcessorTag>,
  Features: HList<IPaymentFeature>,
  SupportedCountries: HList<ICountry>,
  SupportedCurrencies: HList<ICurrency>,
  SupportedMethods: HList<IPaymentMethod>,
> (PhantomData<(
    Tag,
    Features,
    SupportedCountries,
    SupportedCurrencies,
    SupportedMethods)>);

pub type PaymentProcessor<
  Tag,
  Features,
  SupportedCountries,
  SupportedCurrencies,
  SupportedMethods,
> = Wrapper<IPaymentProcessor,
  PaymentProcessorImpl<Tag, Features, SupportedCountries, SupportedCurrencies, SupportedMethods>>;

// Actual data

pub struct USDImpl;
pub struct EURImpl;
pub struct GBPImpl;

pub type USD = Wrapper<ICurrency, USDImpl>;
pub type EUR = Wrapper<ICurrency, EURImpl>;
pub type GBP = Wrapper<ICurrency, GBPImpl>;

pub struct USImpl;
pub struct UKImpl;
pub struct FRImpl;

pub type US = Wrapper<ICountry, USImpl>;
pub type UK = Wrapper<ICountry, UKImpl>;
pub type FR = Wrapper<ICountry, FRImpl>;

pub struct OTPImpl;
pub struct BiometricImpl;
pub struct ThreeDSecureImpl;

pub type OTP = Wrapper<IAuthMethod, OTPImpl>;
pub type Biometric = Wrapper<IAuthMethod, BiometricImpl>;
pub type ThreeDSecure = Wrapper<IAuthMethod, ThreeDSecureImpl>;

pub struct CardTagImpl;
pub type CardTag = Wrapper<IPaymentMethodTag, CardTagImpl>;

pub type CardDataSchema = Schema<tl_list![IField,
  Field<tl_str!("number"), StringType>,
  Field<tl_str!("expiry_month"), IntType>,
  Field<tl_str!("expiry_year"), IntType>,
  Field<tl_str!("cvc"), IntType>,
  OptionalField<tl_str!("holder_name"), StringType>
]>;

pub type Card = PaymentMethod<
    CardTag,
    CardDataSchema,
    tl_list![IPaymentMethodTag],
    true
    >;


pub struct BnplTagImpl;
pub type BnplTag = Wrapper<IPaymentMethodTag, BnplTagImpl>;

pub type BnplSchema = Schema<tl_list![IField,
    Field<tl_str!("name"), StringType>,
    Field<tl_str!("provider"), StringType>,
    Field<tl_str!("bnpl_type"), StringType>,
    Field<tl_str!("currency"), CurrencyType>
    // Other fields
    // Field<tl_str!("installments"), IntType>,
    // Field<tl_str!("interest_rate"), StringType>,
    // Field<tl_str!("down_payment"), StringType>,
    // Field<tl_str!("repayment_start_date"), DateType>,
    // Field<tl_str!("repayment_frequency"), StringType>,
    // Field<tl_str!("penalty_fee"), StringType>,
    // Field<tl_str!("credit_score_check"), BoolType>,
    // Field<tl_str!("grace_period"), StringType>,
    // SchemaField<tl_str!("risk_check"), Schema<tl_list![
    //     Field<tl_str!("fraud_detection"), BoolType>,
    //     Field<tl_str!("geolocation_verification"), BoolType>
    // ]>>
]>;

pub type BNPL = PaymentMethod<
  BnplTag,
  BnplSchema,
  tl_list![IPaymentMethodTag, CardTag],
  false
  >;


// Demo payment processor

pub struct DemoPaymentProcessorTagImpl;
pub type DemoPaymentProcessorTag
  = Wrapper<IPaymentProcessorTag, DemoPaymentProcessorTagImpl>;

pub type DemoPaymentProcessor = PaymentProcessor<
  DemoPaymentProcessorTag,
  tl_list![IPaymentFeature,
    PaymentFeature<UK, Card, USD, ThreeDSecure>,
    PaymentFeature<UK, Card, EUR, ThreeDSecure>,
    PaymentFeature<US, BNPL, USD, Biometric>
  ],
  tl_list![ICountry, US],
  tl_list![ICurrency, USD, EUR, GBP],
  tl_list![IPaymentMethod, Card, BNPL],
  >;

// Paypal demo payment processor

pub struct PayPalTagImpl;
pub type PayPalTag = Wrapper<IPaymentMethodTag, PayPalTagImpl>;

pub type PayPalDataSchema = Schema<tl_list![IField,
  Field<tl_str!("email"), StringType>,
  Field<tl_str!("account_id"), StringType>,
  OptionalField<tl_str!("phone_number"), StringType>
]>;

pub type PayPal = PaymentMethod<
    PayPalTag,
    PayPalDataSchema,
    tl_list![IPaymentMethodTag],
    true
    >;

pub struct PayPalProcessorTagImpl;
pub type PayPalProcessorTag = Wrapper<IPaymentProcessorTag, PayPalProcessorTagImpl>;

pub type PayPalProcessor = PaymentProcessor<
  PayPalProcessorTag,
  tl_list![IPaymentFeature,
    PaymentFeature<US, PayPal, USD, OTP>,
    PaymentFeature<UK, PayPal, GBP, ThreeDSecure>,
    PaymentFeature<FR, PayPal, EUR, Biometric>
  ],
  tl_list![ICountry, US, UK, FR],
  tl_list![ICurrency, USD, EUR, GBP],
  tl_list![IPaymentMethod, PayPal],
    >;


//  Introspection to string ////////////////////////

pub struct Introspect;

// list introspection
impl<I>
  Eval<Introspect, String>
  for TlN_<I>
{
  fn eval() -> String {
    "".to_string()
  }
}

impl<I, Item, Tail>
  Eval<Introspect, String>
  for TlC_<I, Item, Tail>
  where
    Item: IInterface<I> + Eval<Introspect, String>,
    Tail: HList<I> + Eval<Introspect, String>
{
  fn eval() -> String {
    "\n    - ".to_string()
      + &Item::eval()
      + &Tail::eval()
  }
}

// Domain notions introspection

impl Eval<Introspect, String> for Wrapper<ICountry, USImpl> {
  fn eval() -> String {
    "US".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<ICountry, UKImpl> {
  fn eval() -> String {
    "UK".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<ICountry, FRImpl> {
  fn eval() -> String {
    "FR".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<ICurrency, USDImpl> {
  fn eval() -> String {
    "USD".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<ICurrency, EURImpl> {
  fn eval() -> String {
    "EUR".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<ICurrency, GBPImpl> {
  fn eval() -> String {
    "GBP".to_string()
  }
}

impl Eval<Introspect, String>
for Wrapper<IPaymentProcessorTag, DemoPaymentProcessorTagImpl> {
  fn eval() -> String {
    "DemoPaymentProcessor".to_string()
  }
}

impl Eval<Introspect, String>
for Wrapper<IPaymentProcessorTag, PayPalProcessorTagImpl> {
  fn eval() -> String {
    "PayPalProcessor".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<IPaymentMethodTag, CardTagImpl> {
  fn eval() -> String {
    "Card".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<IPaymentMethodTag, BnplTagImpl> {
  fn eval() -> String {
    "BNPL".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<IPaymentMethodTag, PayPalTagImpl> {
  fn eval() -> String {
    "PayPal".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<IAuthMethod, OTPImpl> {
  fn eval() -> String {
    "OTP".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<IAuthMethod, BiometricImpl> {
  fn eval() -> String {
    "Biometric".to_string()
  }
}

impl Eval<Introspect, String> for Wrapper<IAuthMethod, ThreeDSecureImpl> {
  fn eval() -> String {
    "3D Secure".to_string()
  }
}

type PaymentMethodIntrospection = HashMap<String, String>;

impl
  Eval<Introspect, PaymentMethodIntrospection>
  for TlN_<IPaymentMethod>
{
  fn eval() -> PaymentMethodIntrospection {
    HashMap::new()
  }
}

impl<Item, Tail>
  Eval<Introspect, PaymentMethodIntrospection>
  for TlC_<IPaymentMethod, Item, Tail>
  where
    Item: IInterface<IPaymentMethod> + Eval<Introspect, (String, String)>,
    Tail: HList<IPaymentMethod> + Eval<Introspect, PaymentMethodIntrospection>
{
  fn eval() -> PaymentMethodIntrospection {
    let mut introspection = Tail::eval();
    let (key, value) = Item::eval();
    introspection.insert(key, value);
    introspection
  }
}

pub struct ListTags;

impl
  Eval<ListTags, String>
  for TlN_<IPaymentMethodTag>
{
  fn eval() -> String {
    "".to_string()
  }
}

impl<Item, Tail>
  Eval<ListTags, String>
  for TlC_<IPaymentMethodTag, Item, Tail>
  where
    Item: IInterface<IPaymentMethodTag> + Eval<Introspect, String>,
    Tail: HList<IPaymentMethodTag> + Eval<ListTags, String>
{
  fn eval() -> String {
    let tail_is_not_empty = Tail::eval().len() > 0;
    let tail = Tail::eval();
    Item::eval() + if tail_is_not_empty { ", " } else { "" } + &tail
  }
}

pub struct CountFields;

impl
  Eval<CountFields, u32>
  for TlN_<IField>
{
  fn eval() -> u32 {
    0
  }
}

impl<Item, Tail>
  Eval<CountFields, u32>
  for TlC_<IField, Item, Tail>
  where
    Item: IInterface<IField>,
    Tail: HList<IField> + Eval<CountFields, u32>
{
  fn eval() -> u32 {
    1 + Tail::eval()
  }
}

impl <Fields>
  Eval<CountFields, u32>
  for Wrapper<ISchema, SchemaImpl<Fields>>
  where
    Fields: HList<IField> + Eval<CountFields, u32>
{
  fn eval() -> u32 {
    Fields::eval()
  }
}

impl<Tag, DataSchema, FundSource, const REQUIRES_AUTH: bool>
  Eval<Introspect, (String, String)>
  for Wrapper<IPaymentMethod,
    PaymentMethodImpl<Tag, DataSchema, FundSource, REQUIRES_AUTH>>
  where
    Tag: IInterface<IPaymentMethodTag> + Eval<Introspect, String>,
    DataSchema: IInterface<ISchema> + Eval<CountFields, u32>,
    FundSource: HList<IPaymentMethodTag> + Eval<ListTags, String>,
{
  fn eval() -> (String, String) {
    let mut funded_by = FundSource::eval();
    let funded_by_is_not_empty = funded_by.len() > 0;
    let funded_by = if funded_by_is_not_empty { format!("[Funded by: {}] ", funded_by) } else { "".to_string() };
    let fields_count = DataSchema::eval();
    (Tag::eval(), format!("{}[DataSchema # of fields: {}]", funded_by, fields_count))
  }
}

impl <Country, Method, Currency, AuthMethod>
  Eval<Introspect, String>
  for Wrapper<IPaymentFeature,
    PaymentFeatureImpl<Country, Method, Currency, AuthMethod>>
  where
    Country: IInterface<ICountry> + Eval<Introspect, String>,
    Method: IInterface<IPaymentMethod> + Eval<Introspect, (String, String)>,
    Currency: IInterface<ICurrency> + Eval<Introspect, String>,
    AuthMethod: IInterface<IAuthMethod> + Eval<Introspect, String>
{
  fn eval() -> String {
    let (name, intro) = Method::eval();

    "PaymentFeature: [".to_string()
      + &Country::eval()
      + &"] ".to_string()
      + &name
      + &" [".to_string()
      + &Currency::eval()
      + &"] [".to_string()
      + &AuthMethod::eval()
      + &"]"
  }
}

impl <
  Tag: IInterface<IPaymentProcessorTag>,
  Features: HList<IPaymentFeature>,
  SupportedCountries: HList<ICountry>,
  SupportedCurrencies: HList<ICurrency>,
  SupportedMethods: HList<IPaymentMethod>,
>
  Eval<Introspect, String>
for Wrapper<IPaymentProcessor,
  PaymentProcessorImpl<Tag,
    Features, SupportedCountries, SupportedCurrencies, SupportedMethods>>
  where
    Tag: Eval<Introspect, String>,
    Features: Eval<Introspect, String>,
    SupportedCountries: Eval<Introspect, String>,
    SupportedCurrencies: Eval<Introspect, String>,
    SupportedMethods: Eval<Introspect, PaymentMethodIntrospection>,
{
  fn eval() -> String {
    let mut methods = String::new();
    let mut introspected_methods = String::new();
    let introspection = SupportedMethods::eval();
    for (key, val) in introspection.iter() {
      methods.push_str(&format!("\n    - {}", key));
      introspected_methods.push_str(&format!("\n  - {}: {}", key, val));
    }

    "PaymentProcessor: ".to_string()
      + &Tag::eval()
      + &"\n  Features: ".to_string()
      + &Features::eval()
      + &"\n  SupportedCountries: ".to_string()
      + &SupportedCountries::eval()
      + &"\n  SupportedCurrencies: ".to_string()
      + &SupportedCurrencies::eval()
      + &"\n  SupportedMethods: ".to_string()
      + &methods
      + &"\n  Payment methods: ".to_string()
      + &introspected_methods
  }
}

