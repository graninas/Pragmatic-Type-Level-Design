use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::extensibility::request_builder::*;

use serde::{Serialize, Deserialize};
use serde_json::Value;
use either::Either;
use either::Either::{Left, Right};


// "PayPal" payment processor (just a demo, not a real implementation)


#[derive(Default)]
pub struct PayPalPaymentProcessorConfigBuilder {
  pub client_id: Option<String>,
  pub client_secret: Option<String>,
}

impl PayPalPaymentProcessorConfigBuilder {
  pub fn new() -> Self {
    Self::default()
  }
}

impl IConfigBuilder for PayPalPaymentProcessorConfigBuilder {
  fn set_config(mut self: Box<Self>, tag: ConfigTag, value: Value) -> Box<dyn IConfigBuilder> {
    match tag {
      ConfigTag::ClientId => {
        self.client_id = Some(value.as_str().unwrap().to_string());
      },
      ConfigTag::ClientSecret => {
        self.client_secret = Some(value.as_str().unwrap().to_string());
      },
      _ => {}
    }

    self
  }

  fn build_generic_def(&self) -> GenericPaymentProcessorDef {
    GenericPaymentProcessorDef {
      code: code(),
      details: serde_json::to_value(PayPalProcessorConfig {
        client_id: self.client_id.clone().unwrap(),
        client_secret: self.client_secret.clone().unwrap(),
      }).unwrap(),
    }
  }
}


#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PayPalProcessorConfig {
  pub client_id: String,
  pub client_secret: String,
}

pub fn code() -> PaymentProcessorCode {
  "paypal".to_string()
}

pub fn name() -> String {
  "PayPal".to_string()
}

#[derive(Clone)]
pub struct PayPalProcessor {
  generic_def: GenericPaymentProcessorDef,
  config: PayPalProcessorConfig,
}

impl PayPalProcessor {
  pub fn new(
    generic_def: GenericPaymentProcessorDef,
    config: PayPalProcessorConfig) -> Self {
    PayPalProcessor {
      generic_def,
      config,
    }
  }
}

impl IPaymentProcessor for PayPalProcessor {
  // TODO: replace with actual PayPal payment request type
  // type PaymentRequest = Value;

  fn code(&self) -> PaymentProcessorCode {
    code()
  }

  fn name(&self) -> String {
    name()
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
pub struct PayPalProcessorFactory;

impl IPaymentProcessorFactory for PayPalProcessorFactory {
  fn create(&self) -> Result<Box<dyn IPaymentProcessor>, String> {
    Err("PayPal requires client's config".to_string())
  }

  fn get_config_builder(&self) -> Box<dyn IConfigBuilder> {
    Box::new(PayPalPaymentProcessorConfigBuilder::new())
  }

  fn validate_payment_processor(&self, processor: &GenericPaymentProcessorDef)
  -> Either<ValidationResult, Box<dyn IPaymentProcessor>> {

    if processor.code != code() {
      return Left(ValidationResult::Invalid("Invalid payment processor".to_string()));
    }

    let e_processor_config =
      serde_json::from_value::<PayPalProcessorConfig>(processor.details.clone());

    match e_processor_config {
      Ok(processor_config) => {
        let mut errors = Vec::new();

        // Dummy validation logic

        if processor_config.client_id.is_empty() {
          errors.push("Invalid client ID".to_string());
        }

        if processor_config.client_secret.len() == 0 {
          errors.push("Invalid client secret".to_string());
        }

        if errors.len() > 0 {
          return Left(ValidationResult::ValidationError(errors));
        }

        Right(Box::new(PayPalProcessor::new(
          processor.clone(),
          processor_config)))
        },
        Err(e) => {
          Left(ValidationResult::ParseError(e.to_string()))
        }
      }
  }

  fn clone_boxed(&self) -> Box<dyn IPaymentProcessorFactory> {
    let cloned: Self = self.clone();
    Box::new(cloned)
  }
}
