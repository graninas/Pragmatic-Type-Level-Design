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
pub struct IValue<TypeTag: IInterface<ITypeTag>>
  (PhantomData<TypeTag>);

pub struct StoryImpl<
    Name: IInterface<IName>,
    Scenarios: HList<IScenario>>
    (PhantomData<(Name, Scenarios)>);

pub type Story<Name, Scenarios> =
    Wrapper<IStory, StoryImpl<Name, Scenarios>>;


pub struct ScenarioImpl<
    Name: IInterface<IName>,
    Params: HList<IParam>>
    (PhantomData<(Name, Params)>);

  pub type Scenario<Name, Params> =
  Wrapper<IScenario, ScenarioImpl<Name, Params>>;


pub struct NoCondImpl;
pub type NoCond = Wrapper<ICondition, NoCondImpl>;


pub struct MandatoryParamImpl<
    PName: IInterface<IName>,
    PType: IInterface<ITypeTag>,
    Condition: IInterface<ICondition>>
    (PhantomData<(PName, PType, Condition)>);

pub type MandatoryParam<PName, PType, Condition> =
    Wrapper<IParam, MandatoryParamImpl<PName, PType, Condition>>;

pub struct OptionalParamImpl<
    PName: IInterface<IName>,
    PType: IInterface<ITypeTag>,
    Default: IInterface<IValue<PType>>,
    Condition: IInterface<ICondition>>
    (PhantomData<(PName, PType, Default, Condition)>);

pub type OptionalParam<PName, PType, Default, Condition> =
    Wrapper<IParam, OptionalParamImpl<PName, PType, Default, Condition>>;



mod names {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct PaymentsTag;
    pub struct NormalPaymentTag;

    pub type Payments = Wrapper<IName, PaymentsTag>;
    pub type NormalPayment = Wrapper<IName, NormalPaymentTag>;
}

// Common types

pub struct StringImpl;
pub type StringType = Wrapper<ITypeTag, StringImpl>;

pub struct IntImpl;
pub type IntType = Wrapper<ITypeTag, IntImpl>;

// Payment-specific types

pub struct AmountImpl;
pub type AmountType = Wrapper<ITypeTag, AmountImpl>;

pub struct CurrencyImpl;
pub type CurrencyType = Wrapper<ITypeTag, CurrencyImpl>;

pub struct PaymentMethodImpl;
pub type PaymentMethodType = Wrapper<ITypeTag, PaymentMethodImpl>;

pub struct ConfirmationImpl;
pub type ConfirmationType = Wrapper<ITypeTag, ConfirmationImpl>;

pub struct CaptureMethodImpl;
pub type CaptureMethodType = Wrapper<ITypeTag, CaptureMethodImpl>;

mod params {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct AmountTag;
    pub type Amount = Wrapper<IName, AmountTag>;

    pub struct CurrencyTag;
    pub type Currency = Wrapper<IName, CurrencyTag>;

    pub struct DescriptionTag;
    pub type Description = Wrapper<IName, DescriptionTag>;

    pub struct PaymentMethodTag;
    pub type PaymentMethod = Wrapper<IName, PaymentMethodTag>;

    pub struct ConfirmationTag;
    pub type Confirmation = Wrapper<IName, ConfirmationTag>;

    pub struct CaptureMethodTag;
    pub type CaptureMethod = Wrapper<IName, CaptureMethodTag>;
}

mod vals {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct EmptyStringTag;
    pub type EmptyString = Wrapper<IValue<StringType>, EmptyStringTag>;
}

mod confirmation {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct ManualTag;
    pub type Manual = Wrapper<IValue<ConfirmationType>, ManualTag>;

    pub struct AutomaticTag;
    pub type Automatic = Wrapper<IValue<ConfirmationType>, AutomaticTag>;
}

mod capture_method {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct ManualTag;
    pub type Manual = Wrapper<IValue<CaptureMethodType>, ManualTag>;

    pub struct AutomaticTag;
    pub type Automatic = Wrapper<IValue<CaptureMethodType>, AutomaticTag>;
}



// Extensible payment method
mod payment_methods {
    use tl_interface::Wrapper;
    use crate::{*};

    pub struct IPaymentMethod;

    pub struct CardImpl<
        Number: IInterface<IValue<StringType>>,
        ExpMonth: IInterface<IValue<IntType>>,
        ExpYear: IInterface<IValue<IntType>>,
        Cvv: IInterface<IValue<StringType>>
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


pub struct InstantPaymentMethodsCond;
pub type InstantPaymentMethods = Wrapper<ICondition, InstantPaymentMethodsCond>;


pub type NormalPaymentScenario = Scenario<
    names::NormalPayment,
    tl_list![
        IParam,
        MandatoryParam<params::Amount, AmountType, NoCond>,
        MandatoryParam<params::Currency, CurrencyType, NoCond>,
        MandatoryParam<params::PaymentMethod, PaymentMethodType, InstantPaymentMethods>,
        OptionalParam<params::Description, StringType, vals::EmptyString, NoCond>,
        OptionalParam<params::Confirmation, ConfirmationType, confirmation::Manual, NoCond>,
        OptionalParam<params::CaptureMethod, CaptureMethodType, capture_method::Manual, NoCond>
        ]
    >;







fn main() {
    // let amount           = Param::new("amount",           || {json_payload.amount.clone()});
    // let currency         = Param::new("currency",         || {json_payload.currency.clone()});
    // let description      = Param::new("description",      || {json_payload.description.clone()});
    // let payment_method   = Param::new("payment_method",   || {json_payload.payment_method.clone()});
    // let confirmation     = Param::new("confirmation",     || {json_payload.confirmation.clone()});
    // let capture_method   = Param::new("capture_method",   || {json_payload.capture_method.clone()});
    // // let merchant_api_key = Param::new("merchant_api_key", || {req.headers().get("X-API-KEY")});
    // // let merchant_profile = Param::new("merchant_profile", |api_key| { load_merchant_profile(api_key) });

    // let payments_story = Story::new("Payments");
    // let mut normal_payment_scenario = payments_story.build_scenario("Normal Payment");
    // // let mut buy_now_pay_later_scenario = payments_story.build_scenario("Buy Now, Pay Later");

    // // let rt = Runtime::new();

    // normal_payment_scenario
    //   .param(Requirement::Mandatory(amount), amount_validator);
    // //   .param(Mandatory(payment_method), payment_method_validator)
    // //   .param(Optional(description.default("")), description_validator)
    // //   .param(Mandatory(merchant_api_key), api_key_validator)
    // //   // AG: todo: other validators

    // //   .condition(payment_method, |pm| { pm.is_instant() })
    // //   .step("Load merchant profile",
    // //         out(merchant_profile),
    // //         ||{
    // //             let profile = load_merchant_profile(rt.get(merchant_api_key));
    // //             rt.put(merchant_profile, profile);
    // //           }); //////// how to load and pass actual data?


  println!("Hello, world!");
}
