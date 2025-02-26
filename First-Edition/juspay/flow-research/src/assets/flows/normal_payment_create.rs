use serde_json::json;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::extensibility::request_builder::*;
use crate::domain::services::*;
use crate::assets::flow_templates::generic_payment_create::*;


// "Normal payment flow".
// It accepts a bare minimum of parameters and returns a Payment.

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NormalFlowPaymentData {
  pub amount: Amount,
  pub currency: Currency,
  pub description: String,
  pub order_id: OrderId,
  pub desired_payment_processor: Option<Vec<PaymentProcessorCode>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NormalFlowPaymentResult {
  pub payment_id: PaymentId,
  pub third_party_payment: ThirdPartyPayment,
  pub payment_data: NormalFlowPaymentData,
}

pub fn dummy_normal_flow_payment_data() -> NormalFlowPaymentData {
  NormalFlowPaymentData {
    amount: 0,
    currency: Currency::USD,
    description: "".to_string(),
    order_id: "".to_string(),
    desired_payment_processor: None,
  }
}


// Flow configuration & dependencies
pub struct NormalPaymentCreateFlow {
  processor_factories: PaymentProcessorFactories,
  merchant_manager: Box<dyn IMerchantManager>,
}


pub type NormalPaymentCreateFlowBoxed =
  Box<dyn GenericPaymentCreateFlowTemplate<PaymentData=NormalFlowPaymentData,
                                           PaymentResult=NormalFlowPaymentResult>>;

pub type NormalPaymentCreateFlowResult = Result<NormalFlowPaymentResult, String>;

impl NormalPaymentCreateFlow {
  pub fn new(
    processor_factories: PaymentProcessorFactories,
    merchant_manager: Box<dyn IMerchantManager>,
  ) -> NormalPaymentCreateFlowBoxed {
    Box::new(Self { processor_factories, merchant_manager })
  }
}

impl GenericPaymentCreateFlowTemplate for NormalPaymentCreateFlow {

  type PaymentData = NormalFlowPaymentData;
  type PaymentResult = NormalFlowPaymentResult;

  fn merchant_manager(&mut self) -> &mut dyn IMerchantManager {
    &mut *self.merchant_manager
  }

  fn get_or_create_payment_id(
    &mut self,
    merchant_id: &MerchantId,
    payment_data: &Self::PaymentData,
  ) -> Result<PaymentId, String> {
    Ok(format!("{}-{}",
        merchant_id,
        payment_data.order_id))
  }

  fn register_payment_data(
    &mut self,
    _payment_id: &PaymentId,
    _merchant_id: &MerchantId,
    _payment_data: &Self::PaymentData,
  ) -> Result<(), String>{
    Ok(())
  }

  fn decide_payment_processor(
    &mut self,
    _merchant_profile: &MerchantProfile,
    payment_data: &Self::PaymentData,
  ) -> Result<Box<dyn IPaymentProcessor>, String> {

    let desired_payment_processor = payment_data.desired_payment_processor.clone();
    let factories = &self.processor_factories;

    if let Some(desired_payment_processor) = desired_payment_processor {
      for code in desired_payment_processor {
        if let Some(factory) = factories.get(&code) {
          // Using processors that don't require any configuration.
          // Just a demo.
          let processor = factory.create();
          match processor {
            Ok(p) => return Ok(p),
            Err(_) => continue,
          }
        }
      }
    }

    // If no desired payment processor is specified,
    // or if the desired payment processor is not available,
    // return the first one.
    for (_, factory) in factories.iter() {
      let processor = factory.create();
      match processor {
        Ok(p) => return Ok(p),
        Err(_) => continue,
      }
    }

    Err("No payment processor available".to_string())
  }

  fn make_payment_with_payment_processor(
    &mut self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    _merchant_profile: &MerchantProfile,
    payment_id: &PaymentId,
    payment_data: &Self::PaymentData,
  ) -> Result<Self::PaymentResult, String> {

    let mut request_builder = payment_processor.get_request_builder();
    request_builder = request_builder.set_value(ValueTag::Amount, json!(payment_data.amount));
    request_builder = request_builder.set_value(ValueTag::Currency, json!(payment_data.currency));
    // todo: add more values

    let third_party_payment = payment_processor.process_payment(
      payment_id,
      &request_builder)?;

    Ok(NormalFlowPaymentResult {
      payment_id: payment_id.clone(),
      third_party_payment,
      payment_data: payment_data.clone(),
    })
  }

  fn register_payment(&mut self,
    _payment: &Self::PaymentResult) -> Result<(), String> {
    Ok(())
  }
}




