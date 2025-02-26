use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::extensibility::request_builder::*;

use serde::{Serialize, Deserialize};
use either::Either;
use either::Either::{Left, Right};


// Dummy payment processor

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DummyPaymentProcessorDetails;

#[derive(Clone)]
pub struct DummyPaymentProcessor {
  generic_def: GenericPaymentProcessorDef,
  details: DummyPaymentProcessorDetails,
}

impl DummyPaymentProcessor {
  pub fn new(
    generic_def: GenericPaymentProcessorDef,
    details: DummyPaymentProcessorDetails) -> Self {
    DummyPaymentProcessor {
      generic_def,
      details,
    }
  }
}

pub fn code() -> PaymentProcessorCode {
  "dummy".to_string()
}

pub fn name() -> String {
  "Dummy Payment Processor".to_string()
}

impl IPaymentProcessor for DummyPaymentProcessor {
  fn name(&self) -> String {
    name()
  }

  fn code(&self) -> PaymentProcessorCode {
    code()
  }

  fn generic_def(&self) -> GenericPaymentProcessorDef {
    self.generic_def.clone()
  }

  fn get_request_builder(&self) -> Box<dyn IRequestBuilder> {
    Box::new(GenericRequestBuilder::new())
  }

  fn process_payment(
      &self,
      payment_id: &PaymentId,
      _request_builder: &Box<dyn IRequestBuilder>,
  ) -> Result<ThirdPartyPayment, String> {
    // Dummy implementation
    Ok(ThirdPartyPayment {
      third_party_payment_id: Some(payment_id.clone())
    })
  }

  fn clone_boxed(&self) -> Box<dyn IPaymentProcessor> {
    let cloned: Self = self.clone();
    Box::new(cloned)
  }
}

pub struct DummyPaymentProcessorFactory;

impl IPaymentProcessorFactory for DummyPaymentProcessorFactory {
  fn validate_payment_processor(&self, processor: &GenericPaymentProcessorDef)
    -> Either<ValidationResult, Box<dyn IPaymentProcessor>> {
    if processor.code == code() {
      Right(Box::new(DummyPaymentProcessor::new(
        processor.clone(),
        serde_json::from_value(processor.details.clone()).unwrap()
      )))
    } else {
      Left(ValidationResult::Invalid("Invalid payment processor".to_string()))
    }
  }
}


