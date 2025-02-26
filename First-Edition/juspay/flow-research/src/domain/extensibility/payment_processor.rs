use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::request_builder::*;

use either::Either;
use serde_json::Value;
use std::collections::HashMap;


// Extensible payment processors infrastructure


#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum ConfigTag {
  // Add more tags here
  ClientId,
  ClientSecret,

  SomeOtherTag,
}

pub trait IConfigBuilder {
  fn set_config(self: Box<Self>, tag: ConfigTag, value: Value) -> Box<dyn IConfigBuilder>;
  fn build_generic_def(&self) -> GenericPaymentProcessorDef;
}


// Simplified version of a payment processor API.
// In real code base, this interface will be more complex.
pub trait IPaymentProcessor: Send + Sync {
  fn name(&self) -> String;
  fn code(&self) -> PaymentProcessorCode;
  fn generic_def(&self) -> GenericPaymentProcessorDef;
  fn config_json(&self) -> serde_json::Value;

  fn get_request_builder(&self) -> Box<dyn IRequestBuilder>;

  fn process_payment(
      &self,
      payment_id: &PaymentId,
      request_builder: &Box<dyn IRequestBuilder>
  ) -> Result<ThirdPartyPayment, String>;

  fn clone_boxed(&self) -> Box<dyn IPaymentProcessor>;
}

pub trait IPaymentProcessorFactory: Send + Sync {
  // Raw create without configs and validation.
  // May not be supported by some implementations.
  fn create(&self) -> Result<Box<dyn IPaymentProcessor>, String>;

  // Generic way to build a config.
  fn get_config_builder(&self) -> Box<dyn IConfigBuilder>;

  // Validate and create a payment processor
  // with generic config.
  // Must be supported by all implementations.
  fn validate_payment_processor(&self, payment_processor_def: &GenericPaymentProcessorDef)
    -> Either<ValidationResult, Box<dyn IPaymentProcessor>>;

  fn clone_boxed(&self) -> Box<dyn IPaymentProcessorFactory>;
}

pub type PaymentProcessors = HashMap<PaymentProcessorCode, Box<dyn IPaymentProcessor>>;
pub type PaymentProcessorFactories = HashMap<PaymentProcessorCode, Box<dyn IPaymentProcessorFactory>>;


impl Clone for Box<dyn IPaymentProcessor> {
  fn clone(&self) -> Box<dyn IPaymentProcessor> {
      self.clone_boxed()
  }
}

impl Clone for Box<dyn IPaymentProcessorFactory> {
  fn clone(&self) -> Box<dyn IPaymentProcessorFactory> {
      self.clone_boxed()
  }
}
