// This module contains experiments on a value-level eDSL for defining API flows.

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


// This eDSL is WIP



mod api {
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub struct Amount {
        pub value: i32,
    }

    impl Amount {
        pub fn greater(&self, val: i32) -> bool {
            self.value > val
        }
    }

    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub enum Currency {
        USD,
        EUR,
        GBP,
    }

    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub struct PaymentMethod {
        pub value: String,
    }

    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub enum Confirmation {
        #[serde(rename = "manual")]
        Manual,
        #[serde(rename = "automatic")]
        Automatic,
    }

    #[derive(Serialize, Deserialize, Debug, Clone)]
    pub enum CaptureMethod {
        #[serde(rename = "manual")]
        Manual,
        #[serde(rename = "automatic")]
        Automatic,
    }



    pub struct PaymentsRequest {
        pub amount: Option<Amount>,

        /// The three letter ISO currency code in uppercase. Eg: 'USD' to charge US Dollars
        pub currency: Option<Currency>,

        /// Capture method
        pub capture_method: Option<CaptureMethod>,

        /// Whether to confirm the payment (if applicable). It can be used to completely process a payment by attaching a payment method, setting `confirm=true` and `capture_method = automatic` in the *Payments/Create API* request itself.
        pub confirmation: Option<Confirmation>,

        /// A description for the payment
        pub description: Option<String>,

        pub payment_method: Option<PaymentMethod>,
    }


}



mod params {
    pub struct Amount;
    pub struct Currency;
    pub struct String;
    pub struct PaymentMethod;
    pub struct Confirmation;
    pub struct CaptureMethod;
    pub struct ApiKey;
    pub struct MerchantProfile;
}


struct Param<T, F>
where
    F: Fn() -> T,
{
    name: &'static str,
    value_fn: F,
}

impl<T, F> Param<T, F>
where
    F: Fn() -> T,
{
    fn new(name: &'static str, value_fn: F) -> Self {
        Self { name, value_fn }
    }

    fn get_value(&self) -> T {
        (self.value_fn)()
    }
}

pub enum ValidationError {
    Invalid(String),
    InvalidlyAbsent(String),
}
pub type ValidatorResult<T> = Result<T, ValidationError>;


pub enum Requirement<T> {
    Mandatory(Param<Option<T>, F>),
    Optional(Param<Option<T>, F>),
}

pub fn amount_validator(val: ValidatorResult<api::Amount>)
  -> ValidatorResult<api::Amount> {
    let amount = val?;
    if amount.greater(0) {
        Ok(amount)
    } else {
        Err(ValidationError::Invalid("Amount should be greater than 0.".to_string()))
    }
}



pub struct MerchantProfile {
    pub id: String,
    pub name: String,
    pub email: String,
    pub phone: String,
    pub address: String,
    pub city: String,
    pub state: String,
    pub country: String,
    pub zip: String,
}

type ApiKey = String;
fn load_merchant_profile(api_key: &ApiKey) -> MerchantProfile {
    MerchantProfile {
      id: "merchant123".to_string(),
      name: "John Doe".to_string(),
      email: "john.doe@example.com".to_string(),
      phone: "+1234567890".to_string(),
      address: "123 Main St".to_string(),
      city: "Anytown".to_string(),
      state: "Anystate".to_string(),
      country: "Anycountry".to_string(),
      zip: "12345".to_string(),
  }
}


pub enum Step<T, F> {
    Param(Requirement<T>, Fn() -> ValidatorResult<T>),
    // Condition(Param<T, F>, fn(&T) -> bool),
    // Action(&'static str, Param<T, F>, fn() -> T),
}

pub struct Story {
    name: &'static str,
    scenarios: Vec<Scenario>,
}
pub struct Scenario {
    name: &'static str,
    steps: Vec<Box<dyn Any>>,
}

impl Story {
    pub fn new(name: &'static str) -> Story {
        Story { name }
    }

    pub fn build_scenario(&mut self, name: &'static str) -> &Scenario {
        let scenario = Scenario { name, steps: Vec::new() };
        self.scenarios.push(scenario);
        &self.scenarios.last().unwrap()
    }
}

impl Scenario {
    pub fn param<T, F>(&mut self, param: Param<T, F>, validator: fn(&Requirement<T>) -> ValidatorResult<T>)
    where
        F: Fn() -> T,
    {
        self.steps.push(Box::new(Step::Param(param, validator)));
    }

    // pub fn condition<T, F>(&mut self, param: Param<T, F>, condition: fn(&T) -> bool)
    // where
    //     F: Fn() -> T,
    // {
    //     self.steps.push(Step::Condition(param, condition));
    // }

    // pub fn step(&mut self, name: &'static str, out: Param<T, F>, action: fn() -> T)
    // where
    //     F: Fn() -> T,
    // {
    //     self.steps.push(Step::Action(name, out, action));
    // }
}


fn run() {



    let json_payload = api::PaymentsRequest {
        amount: api::Amount { value: 100 },
        currency: api::Currency::USD,
        capture_method: Some(api::CaptureMethod::Manual),
        confirmation: Some(api::Confirmation::Manual),
        description: Some("Test".to_string()),
        payment_method: Some(api::PaymentMethod { value: "card".to_string() }),
    };

    let req = Request::new();

    let amount           = Param::new("amount",           || {json_payload.amount.clone()});
    let currency         = Param::new("currency",         || {json_payload.currency.clone()});
    let description      = Param::new("description",      || {json_payload.description.clone()});
    let payment_method   = Param::new("payment_method",   || {json_payload.payment_method.clone()});
    let confirmation     = Param::new("confirmation",     || {json_payload.confirmation.clone()});
    let capture_method   = Param::new("capture_method",   || {json_payload.capture_method.clone()});
    // let merchant_api_key = Param::new("merchant_api_key", || {req.headers().get("X-API-KEY")});
    // let merchant_profile = Param::new("merchant_profile", |api_key| { load_merchant_profile(api_key) });

    let payments_story = Story::new("Payments");
    let mut normal_payment_scenario = payments_story.build_scenario("Normal Payment");
    // let mut buy_now_pay_later_scenario = payments_story.build_scenario("Buy Now, Pay Later");

    // let rt = Runtime::new();

    normal_payment_scenario
      .param(Requirement::Mandatory(amount), amount_validator);
    //   .param(Mandatory(payment_method), payment_method_validator)
    //   .param(Optional(description.default("")), description_validator)
    //   .param(Mandatory(merchant_api_key), api_key_validator)
    //   // AG: todo: other validators

    //   .condition(payment_method, |pm| { pm.is_instant() })
    //   .step("Load merchant profile",
    //         out(merchant_profile),
    //         ||{
    //             let profile = load_merchant_profile(rt.get(merchant_api_key));
    //             rt.put(merchant_profile, profile);
    //           });

}
