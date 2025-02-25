use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;

use serde_json::Value;
use serde::{Serialize, Deserialize};
use either::Either;
use either::Either::{Left, Right};


// Dummy payment processor

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DummyPaymentProcessorDetails;

pub struct DummyPaymentProcessor {
  details: DummyPaymentProcessorDetails,
}

impl DummyPaymentProcessor {
  pub fn new(details: DummyPaymentProcessorDetails) -> Self {
    DummyPaymentProcessor {
      details,
    }
  }
}

pub fn code() -> String {
  "dummy".to_string()
}

pub fn name() -> String {
  "Dummy Payment Processor".to_string()
}


impl IPaymentProcessor for DummyPaymentProcessor {
  fn name(&self) -> String {
    name()
  }

  fn code(&self) -> String {
    code()
  }

  fn process_payment(
    &self,
    _customer_profile: &CustomerProfile,
    _merchant_profile: &MerchantProfile,
    payment_id: &PaymentId,
    _payment_data: &PaymentData,
    _order_metadata: &OrderMetaData,
  ) -> Result<ThirdPartyPayment, String> {

    // Dummy implementation
    Ok(ThirdPartyPayment {
        third_party_payment_id: Some(payment_id.clone()),
    })

  }
}

pub struct DummyPaymentProcessorFactory;

impl IPaymentProcessorFactory for DummyPaymentProcessorFactory {
  fn validate_payment_processor(&self, processor: &GenericPaymentProcessor)
    -> Either<ValidationResult, Box<dyn IPaymentProcessor>> {
    if processor.code == code() {
      Right(Box::new(DummyPaymentProcessor::new(
        serde_json::from_value(processor.details.clone()).unwrap()
      )))
    } else {
      Left(ValidationResult::Invalid("Invalid payment processor".to_string()))
    }
  }
}
