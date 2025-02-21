
#[cfg(test)]
mod tests {
  use super::*;

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

  // Currencies

  pub struct ICurrency;

  pub struct USDImpl;
  pub struct EURImpl;
  pub struct GBPImpl;

  pub type USD = Wrapper<ICurrency, USDImpl>;
  pub type EUR = Wrapper<ICurrency, EURImpl>;
  pub type GBP = Wrapper<ICurrency, GBPImpl>;

  // Countries

  pub struct ICountry;

  pub struct USImpl;
  pub struct UKImpl;
  pub struct FRImpl;

  pub type US = Wrapper<ICountry, USImpl>;
  pub type UK = Wrapper<ICountry, UKImpl>;
  pub type FR = Wrapper<ICountry, FRImpl>;

  // Authentication methods

  pub struct IAuthMethod;

  pub struct OTPImpl;
  pub struct BiometricImpl;
  pub struct ThreeDSecureImpl;

  pub type OTP = Wrapper<IAuthMethod, OTPImpl>;
  pub type Biometric = Wrapper<IAuthMethod, BiometricImpl>;
  pub type ThreeDSecure = Wrapper<IAuthMethod, ThreeDSecureImpl>;

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



  pub struct DemoPaymentProcessorTagImpl;
  pub type DemoPaymentProcessorTag
    = Wrapper<IPaymentProcessorTag, DemoPaymentProcessorTagImpl>;

  pub type DemoPaymentProcessor = PaymentProcessor<
    DemoPaymentProcessorTag,
    tl_list![
      PaymentFeature<UK, Card, USD, ThreeDSecure>,
      PaymentFeature<UK, Card, EUR, ThreeDSecure>,
      PaymentFeature<US, BNPL, USD, Biometric>
    ],
    tl_list![US],
    tl_list![USD, EUR, GBP],
    tl_list![Card, BNPL],
    >;







    //   pub struct CardDataImpl<
    //   Name: TlStr,
    //   Number: TlStr,
    //   Currency: IInterface<ICurrency>,
    //   const u16: EXPIRY_DATE,
    //   const u8: CVV,
    // >;

    // pub type CardData<
    //   Name,
    //   Number,
    //   Currency,
    //   const u16: EXPIRY_DATE,
    //   const u8: CVV,
    //   > = Wrapper<IPaymentMethodData,
    //   CardDataImpl<Name, Number, Currency, EXPIRY_DATE, CVV>>;



  #[test]
  fn test_type_level_edsl2() {



  }

}









// 3. Rules for Combining Authentication Methods
// When multiple authentication requirements are present, follow these rules:

// ✅ Rule 1: Enforce the Strongest Regulatory Authentication
// If a country mandates an authentication method, apply it first.
// Example: EU (PSD2) → Always apply 3D Secure 2 for cards.
// ✅ Rule 2: If No Country Regulations, Prioritize Cross-Border Auth
// If the transaction is cross-border, apply a strong authentication method unless the payment method already has built-in security.
// Example: USD → INR → Apply OTP.
// ✅ Rule 3: If No Other Rules Apply, Use the Payment Method’s Own Auth
// If the payment method itself has authentication (e.g., Apple Pay uses Face ID, Credit Cards use 3D Secure), rely on that.
// Example: Apple Pay in USA → Use biometric authentication.
// ✅ Rule 4: Avoid Combining Multiple Auth Methods Unless Necessary
// Never enforce multiple redundant authentication methods (e.g., 3D Secure + OTP) unless legally required.
// Example: If Germany requires 3D Secure but the currency suggests OTP, only apply 3D Secure.
