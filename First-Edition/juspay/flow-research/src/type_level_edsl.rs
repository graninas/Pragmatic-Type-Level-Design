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

use actix_http::Request;




pub struct ITypeTag;
pub struct IName;
pub struct IStory;
pub struct IScenario;
pub struct ICondition;
pub struct IParam;
pub struct IVar<TypeTag: IInterface<ITypeTag>>
  (PhantomData<TypeTag>);
pub struct IVal<TypeTag: IInterface<ITypeTag>>
  (PhantomData<TypeTag>);
pub struct IStep;
pub struct IAction;

pub struct VarImpl<const IDX: u8>;


pub struct NoCondImpl;
pub type NoCond = Wrapper<ICondition, NoCondImpl>;

pub struct GreaterImpl<const N: i32>;
pub type Greater<const N: i32> = Wrapper<ICondition, GreaterImpl<N>>;


pub struct StoryImpl<
    Name: IInterface<IName>,
    Scenarios: HList<IScenario>>
    (PhantomData<(Name, Scenarios)>);

pub type Story<Name, Scenarios> =
    Wrapper<IStory, StoryImpl<Name, Scenarios>>;


pub struct ScenarioImpl<
    Name: IInterface<IName>,
    Params: HList<IParam>,
    Steps: HList<IStep>>
    (PhantomData<(Name, Params, Steps)>);

pub type Scenario<Name, Params, Steps> =
    Wrapper<IScenario, ScenarioImpl<Name, Params, Steps>>;


pub struct MandatoryParamImpl<
    PName: IInterface<IName>,
    PType: IInterface<ITypeTag>,
    Var: IInterface<IVar<PType>>,
    Condition: IInterface<ICondition>>
    (PhantomData<(PName, PType, Var, Condition)>);

pub type MandatoryParam<PName, PType, Var, Condition> =
    Wrapper<IParam, MandatoryParamImpl<PName, PType, Var, Condition>>;

pub struct OptionalParamImpl<
    PName: IInterface<IName>,
    PType: IInterface<ITypeTag>,
    Var: IInterface<IVar<PType>>,
    Default: IInterface<IVal<PType>>>
    (PhantomData<(PName, PType, Var, Default)>);

pub type OptionalParam<PName, PType, Var, Default> =
    Wrapper<IParam, OptionalParamImpl<PName, PType, Var, Default>>;

pub struct MandatoryAuthImpl<
    PName: IInterface<IName>,
    PType: IInterface<ITypeTag>,
    Var: IInterface<IVar<PType>>>
    (PhantomData<(PName, PType, Var)>);

pub type MandatoryAuth<PName, PType, Var> =
    Wrapper<IParam, MandatoryAuthImpl<PName, PType, Var>>;

pub struct StepImpl<
    Action: IInterface<IAction>,
    Out: IInterface<IName>>
    (PhantomData<(Action, Out)>);

pub type Step<Action, Out> =
    Wrapper<IStep, StepImpl<Action, Out>>;

pub struct ConditionalStepImpl<
    Condition: IInterface<ICondition>,
    TrueSteps: HList<IStep>,
    FalseSteps: HList<IStep>>
    (PhantomData<(Condition, TrueSteps, FalseSteps)>);

pub type ConditionalStep<Condition, TrueSteps, FalseSteps> =
    Wrapper<IStep, ConditionalStepImpl<Condition, TrueSteps, FalseSteps>>;

// This step stops the scenario execution if the condition is not met.
pub struct GuardStepImpl<
    Condition: IInterface<ICondition>,
    Steps: HList<IStep>>
    (PhantomData<(Condition, Steps)>);

pub type GuardStep<Condition, Steps> =
    Wrapper<IStep, GuardStepImpl<Condition, Steps>>;

// Common types

pub struct StringImpl;
pub type StringType = Wrapper<ITypeTag, StringImpl>;

pub struct IntImpl;
pub type IntType = Wrapper<ITypeTag, IntImpl>;

// Payment-specific types

pub struct AmountTypeImpl;
pub type AmountType = Wrapper<ITypeTag, AmountTypeImpl>;

pub struct CurrencyTypeImpl;
pub type CurrencyType = Wrapper<ITypeTag, CurrencyTypeImpl>;

pub struct PaymentMethodTypeImpl;
pub type PaymentMethodType = Wrapper<ITypeTag, PaymentMethodTypeImpl>;

pub struct ConfirmationTypeImpl;
pub type ConfirmationType = Wrapper<ITypeTag, ConfirmationTypeImpl>;

pub struct CaptureMethodTypeImpl;
pub type CaptureMethodType = Wrapper<ITypeTag, CaptureMethodTypeImpl>;

pub struct MerchantApiKeyTypeImpl;
pub type MerchantApiKeyType = Wrapper<ITypeTag, MerchantApiKeyTypeImpl>;

pub struct MerchantProfileTypeImpl;
pub type MerchantProfileType = Wrapper<ITypeTag, MerchantProfileTypeImpl>;

pub struct PaymentGatewayTypeImpl;
pub type PaymentGatewayType = Wrapper<ITypeTag, PaymentGatewayTypeImpl>;

pub struct PaymentIntentTypeImpl;
pub type PaymentIntentType = Wrapper<ITypeTag, PaymentIntentTypeImpl>;

pub struct PaymentTypeImpl;
pub type PaymentType = Wrapper<ITypeTag, PaymentTypeImpl>;

pub struct PaymentStatusTypeImpl;
pub type PaymentStatusType = Wrapper<ITypeTag, PaymentStatusTypeImpl>;

mod names {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct PaymentsNameTag;
    pub type Payments = Wrapper<IName, PaymentsNameTag>;

    pub struct NormalPaymentNameTag;
    pub type NormalPayment = Wrapper<IName, NormalPaymentNameTag>;

    // These names correspond to the names in the JSON payload.
    // The structs replace tl_str!() macro to keep types nesting limit lower.
    pub struct AmountNameTag;
    pub type Amount = Wrapper<IName, AmountNameTag>;

    // Alternatively, it can be:
    // pub type Amount = tl_str!("amount");
    // Or:
    // pub type Amount = Wrapper<IName, tl_str!("amount")>;

    pub struct CurrencyNameTag;
    pub type Currency = Wrapper<IName, CurrencyNameTag>;

    pub struct DescriptionNameTag;
    pub type Description = Wrapper<IName, DescriptionNameTag>;

    pub struct PaymentMethodNameTag;
    pub type PaymentMethod = Wrapper<IName, PaymentMethodNameTag>;

    pub struct ConfirmationNameTag;
    pub type Confirmation = Wrapper<IName, ConfirmationNameTag>;

    pub struct CaptureMethodNameTag;
    pub type CaptureMethod = Wrapper<IName, CaptureMethodNameTag>;

    pub struct MerchantApiKeyNameTag;
    pub type MerchantApiKey = Wrapper<IName, MerchantApiKeyNameTag>;
}

mod vars {
    use tl_interface::Wrapper;
    use crate::{*};

  // Variable types. These are used to store values during the scenario execution.
  // Indexes are used to distinguish between different variables of the same type.
  // Indexes should not be reused within the same scenario.
    pub type Amount<const IDX: u8>         = Wrapper<IVar<AmountType>, VarImpl<IDX>>;
    pub type Currency<const IDX: u8>       = Wrapper<IVar<CurrencyType>, VarImpl<IDX>>;
    pub type Str<const IDX: u8>            = Wrapper<IVar<StringType>, VarImpl<IDX>>;
    pub type PaymentMethod<const IDX: u8>  = Wrapper<IVar<PaymentMethodType>, VarImpl<IDX>>;
    pub type Confirmation<const IDX: u8>   = Wrapper<IVar<ConfirmationType>, VarImpl<IDX>>;
    pub type CaptureMethod<const IDX: u8>  = Wrapper<IVar<CaptureMethodType>, VarImpl<IDX>>;
    pub type MerchantApiKey<const IDX: u8> = Wrapper<IVar<MerchantApiKeyType>, VarImpl<IDX>>;

    pub type MerchantProfile<const IDX: u8> = Wrapper<IVar<MerchantProfileType>, VarImpl<IDX>>;
    pub type PaymentGateway<const IDX: u8>  = Wrapper<IVar<PaymentGatewayType>, VarImpl<IDX>>;
    pub type PaymentIntent<const IDX: u8>   = Wrapper<IVar<PaymentIntentType>, VarImpl<IDX>>;
    pub type Payment<const IDX: u8>         = Wrapper<IVar<PaymentType>, VarImpl<IDX>>;
    pub type PaymentStatus<const IDX: u8>   = Wrapper<IVar<PaymentStatusType>, VarImpl<IDX>>;
}

mod vals {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct EmptyStringTag;
    pub type EmptyString = Wrapper<IVal<StringType>, EmptyStringTag>;
}

mod confirmation {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct ManualTag;
    pub type Manual = Wrapper<IVal<ConfirmationType>, ManualTag>;

    pub struct AutomaticTag;
    pub type Automatic = Wrapper<IVal<ConfirmationType>, AutomaticTag>;
}

mod capture_method {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct ManualTag;
    pub type Manual = Wrapper<IVal<CaptureMethodType>, ManualTag>;

    pub struct AutomaticTag;
    pub type Automatic = Wrapper<IVal<CaptureMethodType>, AutomaticTag>;
}



// Extensible payment method
mod payment_methods {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct IPaymentMethod;

    pub struct CardImpl<
        Number: IInterface<IVal<StringType>>,
        ExpMonth: IInterface<IVal<IntType>>,
        ExpYear: IInterface<IVal<IntType>>,
        Cvv: IInterface<IVal<StringType>>
        >
    (PhantomData<(Number, ExpMonth, ExpYear, Cvv)>);
    pub type Card<Number, ExpMonth, ExpYear, Cvv> =
        Wrapper<IPaymentMethod, CardImpl<Number, ExpMonth, ExpYear, Cvv>>;

    pub struct NetBankingImpl;
    pub type NetBanking = Wrapper<IPaymentMethod, NetBankingImpl>;

    pub struct WalletImpl;
    pub type Wallet = Wrapper<IPaymentMethod, WalletImpl>;
}


// Stories and scenarios schema

pub type PaymentsStory = Story<
    names::Payments,
    tl_list![
        IScenario,
        NormalPaymentScenario]>;

pub struct IsInstantPaymentMethodCond <P: IInterface<IVar<PaymentMethodType>>>
  (PhantomData<P>);

pub type IsInstantPaymentMethod<P>
  = Wrapper<ICondition, IsInstantPaymentMethodCond<P>>;

pub struct PaymentIntentRequiredImpl<
  Gateway: IInterface<IVar<PaymentGatewayType>>
  >(PhantomData<Gateway>);

pub type PaymentIntentRequired<Gateway> =
  Wrapper<ICondition, PaymentIntentRequiredImpl<Gateway>>;

// Actions
pub struct LoadMerchantProfileImpl<
  ApiKey: IInterface<IVar<MerchantApiKeyType>>
  >(PhantomData<ApiKey>);
pub type LoadMerchantProfile<ApiKey> =
  Wrapper<IAction, LoadMerchantProfileImpl<ApiKey>>;

pub struct ChoosePaymentGatewayImpl<
  Profile: IInterface<IVar<MerchantProfileType>>
  >(PhantomData<Profile>);
pub type ChoosePaymentGateway<Profile> =
  Wrapper<IAction, ChoosePaymentGatewayImpl<Profile>>;

pub struct CreatePaymentIntentImpl<
  Gateway: IInterface<IVar<PaymentGatewayType>>
  >(PhantomData<Gateway>);

pub type CreatePaymentIntent<Gateway> =
  Wrapper<IAction, CreatePaymentIntentImpl<Gateway>>;

pub struct AttachPaymentMethodImpl<
  Gateway: IInterface<IVar<PaymentGatewayType>>,
  Payment: IInterface<IVar<PaymentIntentType>>
  >(PhantomData<(Gateway, Payment)>);

pub type AttachPaymentMethod<Gateway, Payment> =
  Wrapper<IAction, AttachPaymentMethodImpl<Gateway, Payment>>;

pub struct AuthorizePaymentImpl<
  Gateway: IInterface<IVar<PaymentGatewayType>>,
  Payment: IInterface<IVar<PaymentType>>
  >(PhantomData<(Gateway, Payment)>);

pub type AuthorizePayment<Gateway, Payment> =
  Wrapper<IAction, AuthorizePaymentImpl<Gateway, Payment>>;

pub struct CreatePaymentImpl<
  Gateway: IInterface<IVar<PaymentGatewayType>>
  >(PhantomData<Gateway>);

pub type CreatePayment<Gateway> =
  Wrapper<IAction, CreatePaymentImpl<Gateway>>;


// Scenarios

pub type NormalPaymentScenario = Scenario<
    names::NormalPayment,
    tl_list![
        IParam,
        MandatoryParam<names::Amount,         AmountType,        vars::Amount<1>,        Greater<0>>,
        MandatoryParam<names::Currency,       CurrencyType,      vars::Currency<1>,      NoCond>,
        MandatoryParam<names::PaymentMethod,  PaymentMethodType, vars::PaymentMethod<1>, NoCond>,
        OptionalParam <names::Description,    StringType,        vars::Str<1>,           vals::EmptyString>,
        OptionalParam <names::Confirmation,   ConfirmationType,  vars::Confirmation<1>,  confirmation::Manual>,
        OptionalParam <names::CaptureMethod,  CaptureMethodType, vars::CaptureMethod<1>, capture_method::Manual>,
        MandatoryAuth <names::MerchantApiKey, StringType,        vars::MerchantApiKey<1>>
        ],

    GuardStep<
      IsInstantPaymentMethod<vars::PaymentMethod<1>>,
      tl_list![IStep,
        Step<LoadMerchantProfile<vars::MerchantApiKey<1>>, vars::MerchantProfile<1>>,
        Step<ChoosePaymentGateway<vars::MerchantProfile<1>>, vars::PaymentGateway<1>>,
        ConditionalStep<
          PaymentIntentRequired<vars::PaymentGateway<1>>,
          tl_list![IStep,
            Step<CreatePaymentIntent<vars::PaymentGateway<1>>, vars::PaymentIntent<1>>,
            Step<AttachPaymentMethod<vars::PaymentGateway<1>, vars::PaymentIntent<1>>, vars::Payment<1>>,
            Step<AuthorizePayment<vars::PaymentGateway<1>, vars::Payment<1>>, vars::PaymentStatus<1>>
          ],
          tl_list![IStep,
            Step<CreatePayment<vars::PaymentGateway<1>>, vars::Payment<1>>,
            Step<AuthorizePayment<vars::PaymentGateway<1>, vars::Payment<1>>, vars::PaymentStatus<1>>
          ]
        >
      ]
    >

  >;




fn main() {
  println!("Hello, world!");
}
