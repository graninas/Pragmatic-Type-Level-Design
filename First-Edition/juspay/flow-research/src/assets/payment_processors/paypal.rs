use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::extensibility::request_builder::*;

use serde::{Serialize, Deserialize};
use either::Either;
use either::Either::{Left, Right};


#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PayPalProcessorDetails {
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
  details: PayPalProcessorDetails,
}

impl PayPalProcessor {
  pub fn new(
    generic_def: GenericPaymentProcessorDef,
    details: PayPalProcessorDetails) -> Self {
    PayPalProcessor {
      generic_def,
      details,
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

pub struct PayPalProcessorFactory;

impl IPaymentProcessorFactory for PayPalProcessorFactory {
  fn validate_payment_processor(&self, processor: &GenericPaymentProcessorDef)
    -> Either<ValidationResult, Box<dyn IPaymentProcessor>> {

      if processor.code != code() {
        return Left(ValidationResult::Invalid("Invalid payment processor".to_string()));
      }

      let e_processor_details =
        serde_json::from_value::<PayPalProcessorDetails>(processor.details.clone());

      match e_processor_details {
        Ok(processor_details) => {
          let mut errors = Vec::new();

          // Dummy validation logic

          if processor_details.client_id.is_empty() {
            errors.push("Invalid client ID".to_string());
          }

          if processor_details.client_secret.len() == 0 {
            errors.push("Invalid client secret".to_string());
          }

          if errors.len() > 0 {
            return Left(ValidationResult::ValidationError(errors));
          }

          Right(Box::new(PayPalProcessor::new(
            processor.clone(),
            processor_details)))
        },
        Err(e) => {
          Left(ValidationResult::ParseError(e.to_string()))
        }
    }
  }
}
