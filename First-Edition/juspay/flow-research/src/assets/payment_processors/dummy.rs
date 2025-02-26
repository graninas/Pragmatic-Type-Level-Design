use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::extensibility::request_builder::*;

use serde::{Serialize, Deserialize};
use serde_json::Value;
use either::Either;
use either::Either::{Left, Right};


// Dummy payment processor

#[derive(Default)]
pub struct DummyPaymentProcessorConfigBuilder;

impl DummyPaymentProcessorConfigBuilder {
  pub fn new() -> Self {
    Self::default()
  }
}

impl IConfigBuilder for DummyPaymentProcessorConfigBuilder {
  fn set_config(self: Box<Self>,
    _tag: ConfigTag,
    _value: Value) -> Box<dyn IConfigBuilder> {
    self
  }

  fn build_generic_def(&self) -> GenericPaymentProcessorDef {
    GenericPaymentProcessorDef {
      code: code(),
      details: serde_json::to_value(DummyPaymentProcessorConfig).unwrap(),
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DummyPaymentProcessorConfig;

#[derive(Clone)]
pub struct DummyPaymentProcessor {
  generic_def: GenericPaymentProcessorDef,
  config: DummyPaymentProcessorConfig,
}

impl DummyPaymentProcessor {
  pub fn new(
    generic_def: GenericPaymentProcessorDef,
    config: DummyPaymentProcessorConfig) -> Self {
    DummyPaymentProcessor {
      generic_def,
      config,
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

  fn config_json(&self) -> serde_json::Value {
    serde_json::to_value(&self.config).unwrap()
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

#[derive(Clone)]
pub struct DummyPaymentProcessorFactory;

impl IPaymentProcessorFactory for DummyPaymentProcessorFactory {

  fn create(&self) -> Result<Box<dyn IPaymentProcessor>, String> {
    Ok(Box::new(DummyPaymentProcessor::new(
      GenericPaymentProcessorDef {
        code: code(),
        details: serde_json::to_value(DummyPaymentProcessorConfig).unwrap(),
      },
      DummyPaymentProcessorConfig
    )))
  }

  fn get_config_builder(&self) -> Box<dyn IConfigBuilder> {
    Box::new(DummyPaymentProcessorConfigBuilder::new())
  }

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

  fn clone_boxed(&self) -> Box<dyn IPaymentProcessorFactory> {
    let cloned: Self = self.clone();
    Box::new(cloned)
  }
}


