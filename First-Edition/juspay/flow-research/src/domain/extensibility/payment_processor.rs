use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::request_builder::*;

use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};


// Extensible payment processors infrastructure

// Simplified version of a payment processor API.
// In real code base, this interface will be more complex.
pub trait IPaymentProcessor {
  fn name(&self) -> String;
  fn code(&self) -> String;

  fn get_request_builder(&self) -> Box<dyn IRequestBuilder>;

  fn process_payment(
      &self,
      customer_profile: &CustomerProfile,
      merchant_profile: &MerchantProfile,
      payment_id: &PaymentId,
      request: &Value,
      order_metadata: &OrderMetaData,
      // TODO: replace ThirdPartyPayment with an associated type
  ) -> Result<ThirdPartyPayment, String>;
}

pub trait IPaymentProcessorFactory {
  fn validate_payment_processor(&self, payment_processor: &GenericPaymentProcessor)
    -> Either<ValidationResult, Box<dyn IPaymentProcessor>>;
}

