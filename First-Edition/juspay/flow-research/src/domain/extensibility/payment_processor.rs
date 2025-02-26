use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::request_builder::*;

use either::Either;
use std::collections::HashMap;


// Extensible payment processors infrastructure

// Simplified version of a payment processor API.
// In real code base, this interface will be more complex.
pub trait IPaymentProcessor: Send + Sync {
  fn name(&self) -> String;
  fn code(&self) -> PaymentProcessorCode;
  fn generic_def(&self) -> GenericPaymentProcessorDef;

  fn get_request_builder(&self) -> Box<dyn IRequestBuilder>;

  fn process_payment(
      &self,
      payment_id: &PaymentId,
      request_builder: &Box<dyn IRequestBuilder>
  ) -> Result<ThirdPartyPayment, String>;

  fn clone_boxed(&self) -> Box<dyn IPaymentProcessor>;
}

pub trait IPaymentProcessorFactory {
  fn validate_payment_processor(&self, payment_processor_def: &GenericPaymentProcessorDef)
    -> Either<ValidationResult, Box<dyn IPaymentProcessor>>;
}

pub type PaymentProcessors = HashMap<PaymentProcessorCode, Box<dyn IPaymentProcessor>>;
pub type PaymentProcessorFactories = HashMap<PaymentProcessorCode, Box<dyn IPaymentProcessorFactory>>;


impl Clone for Box<dyn IPaymentProcessor> {
    fn clone(&self) -> Box<dyn IPaymentProcessor> {
        self.clone_boxed()
    }
}
