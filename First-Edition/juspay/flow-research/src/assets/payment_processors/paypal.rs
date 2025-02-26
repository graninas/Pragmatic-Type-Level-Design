use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::extensibility::request_builder::*;

use serde_json::Value;
use serde::{Serialize, Deserialize};
use either::Either;
use either::Either::{Left, Right};


#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PayPalProcessorDetails {
  pub client_id: String,
  pub client_secret: String,
}

pub fn code() -> String {
  "paypal".to_string()
}

pub fn name() -> String {
  "PayPal".to_string()
}

pub struct PayPalProcessor {
  details: PayPalProcessorDetails,
}

impl PayPalProcessor {
  pub fn new(details: PayPalProcessorDetails) -> Self {
    PayPalProcessor {
      details,
    }
  }
}

impl IPaymentProcessor for PayPalProcessor {
  // TODO: replace with actual PayPal payment request type
  // type PaymentRequest = Value;

  fn code(&self) -> String {
    code()
  }

  fn name(&self) -> String {
    name()
  }

  fn get_request_builder(&self) -> Box<dyn IRequestBuilder<Request = Self::PaymentRequest>> {
    Box::new(GenericRequestBuilder::new())
  }

  fn process_payment(
      &self,
      _customer_profile: &CustomerProfile,
      _merchant_profile: &MerchantProfile,
      payment_id: &PaymentId,
      _payment_request: &Self::PaymentRequest,
      _order_metadata: &OrderMetaData,
  ) -> Result<ThirdPartyPayment, String> {

    // Dummy implementation
    Ok(ThirdPartyPayment {
      third_party_payment_id: Some(payment_id.clone())
    })

  }
}

pub struct PayPalProcessorFactory;

impl IPaymentProcessorFactory for PayPalProcessorFactory {
  fn validate_payment_processor(&self, processor: &GenericPaymentProcessor)
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

          Right(Box::new(PayPalProcessor::new(processor_details)))
        },
        Err(e) => {
          Left(ValidationResult::ParseError(e.to_string()))
        }
    }
  }
}
