use either::Either;
use either::Either::{Left, Right};
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::services::*;
use crate::assets::flow_templates::generic_payment_create::*;
use crate::assets::payment_processors::dummy as ext_pp_dummy;
use crate::application::services::ILogger;


// "Normal payment flow".
// It accepts a bare minimum of parameters and returns a Payment.

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NormalFlowPaymentData {
  pub amount: Amount,
  pub currency: Currency,
  pub description: String,
  pub confirmation: Confirmation,
  pub capture_method: CaptureMethod,
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
    confirmation: Confirmation::Manual,
    capture_method: CaptureMethod::Manual,
  }
}

pub struct NormalPaymentCreateFlow {
  customer_manager: Box<dyn ICustomerManager>,
  merchant_manager: Box<dyn IMerchantManager>,
}


pub type NormalPaymentCreateFlowBoxed =
  Box<dyn GenericPaymentCreateFlowTemplate<PaymentData=NormalFlowPaymentData,
                                           PaymentResult=NormalFlowPaymentResult>>;

pub type NormalPaymentCreateFlowResult = Result<NormalFlowPaymentResult, String>;

impl NormalPaymentCreateFlow {
  pub fn new(
    customer_manager: Box<dyn ICustomerManager>,
    merchant_manager: Box<dyn IMerchantManager>,
  ) -> NormalPaymentCreateFlowBoxed {
    Box::new(Self { customer_manager, merchant_manager })
  }
}

impl GenericPaymentCreateFlowTemplate for NormalPaymentCreateFlow {

  type PaymentData = NormalFlowPaymentData;
  type PaymentResult = NormalFlowPaymentResult;

  fn customer_manager(&mut self) -> &mut dyn ICustomerManager {
    &mut *self.customer_manager
  }

  fn merchant_manager(&mut self) -> &mut dyn IMerchantManager {
    &mut *self.merchant_manager
  }

  fn get_or_create_customer(
    &mut self,
    customer_data: Either<CustomerId, CustomerDetails>,
  ) -> Result<CustomerProfile, String> {
    match customer_data {
      Either::Left(customer_id) => {
        let mb_customer = self.customer_manager.get_customer(customer_id);
        match mb_customer {
          Ok(Some(customer_profile)) => Ok(customer_profile),
          Ok(None) => Err("Customer not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(customer_details) => {
        self.customer_manager.create_customer(customer_details)
      }
    }
  }

  fn get_or_create_merchant(
    &mut self,
    merchant_data: Either<MerchantId, MerchantDetails>,
  ) -> Result<MerchantProfile, String> {
    match merchant_data {
      Either::Left(merchant_id) => {
        let mb_merchant = self.merchant_manager.get_merchant(merchant_id);
        match mb_merchant {
          Ok(Some(merchant_profile)) => Ok(merchant_profile),
          Ok(None) => Err("Merchant not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(merchant_details) => {
        self.merchant_manager.create_merchant(merchant_details)
      }
    }
  }

  fn get_or_create_payment_id(
    &mut self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<PaymentId, String> {
    Ok(format!("{}-{}-{}",
        customer_id,
        merchant_id,
        order_metadata.order_id))
  }

  // fn register_order_metadata(
  //   &mut self,
  //   payment_id: &PaymentId,
  //   customer_id: &CustomerId,
  //   merchant_id: &MerchantId,
  //   order_metadata: &OrderMetaData,
  // ) -> Result<(), String> {
  //   Ok(())
  // }

  // fn decide_payment_processor(
  //   &mut self,
  //   merchant_profile: &MerchantProfile,
  //   payment_data: &Self::PaymentData,
  //   order_metadata: &OrderMetaData,
  // ) -> Result<Box<dyn IPaymentProcessor>, String> {
  //   Ok(Box::new(DummyPaymentProcessor))
  // }

  // fn make_payment_with_payment_processor(
  //   &mut self,
  //   payment_processor: &Box<dyn IPaymentProcessor>,
  //   customer_profile: &CustomerProfile,
  //   merchant_profile: &MerchantProfile,
  //   payment_id: &PaymentId,
  //   payment_data: &Self::PaymentData,
  //   order_metadata: &OrderMetaData,
  // ) -> Result<Self::PaymentResult, String> {
  //   let third_party_payment = payment_processor.process_payment(
  //     customer_profile,
  //     merchant_profile,
  //     payment_id,
  //     payment_data,
  //     order_metadata)?;

  //   Ok(NormalFlowPaymentResult {
  //     payment_id: payment_id.clone(),
  //     third_party_payment,
  //     payment_data: payment_data.clone(),
  //   })
  // }

  // fn register_payment(&mut self, payment: &Self::PaymentResult) -> Result<(), String> {
  //   Ok(())
  // }
}




