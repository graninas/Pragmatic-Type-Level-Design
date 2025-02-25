use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_method::*;

use either::Either;
use either::Either::{Left, Right};
use serde_json::Value;
use serde::{Serialize, Deserialize};


#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum PayPalProvider {
  PayPal,
  BrainTree,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PayPalPayerDetails {
  pub payer_id: String,
  pub payer_email: String,
  pub provider: PayPalProvider,
}

pub fn code() -> String {
  "paypal".to_string()
}

pub struct PayPalPaymentMethod {
  payer_details: PayPalPayerDetails,
}

impl PayPalPaymentMethod {
  pub fn new(payer_details: PayPalPayerDetails) -> Self {
    PayPalPaymentMethod {
      payer_details,
    }
  }
}

impl IPaymentMethod for PayPalPaymentMethod {
  fn code(&self) -> String {
    code()
  }
}

pub struct PayPalPaymentMethodFactory;

impl IPaymentMethodFactory for PayPalPaymentMethodFactory {
  fn validate_payment_method(&self, method: &GenericPaymentMethod)
    -> Either<ValidationResult, Box<dyn IPaymentMethod>> {

      if method.code != code() {
        return Left(ValidationResult::Invalid("Invalid payment method".to_string()));
      }

      let e_payer_details =
        serde_json::from_value::<PayPalPayerDetails>(method.details.clone());

      match e_payer_details {
        Ok(payer_details) => {
          let mut errors = Vec::new();

          // Dummy validation logic

          if payer_details.payer_id.is_empty() {
            errors.push("Invalid payer ID".to_string());
          }

          if payer_details.payer_email.len() == 0 {
            errors.push("Invalid payer email".to_string());
          }

          if errors.len() > 0 {
            return Left(ValidationResult::ValidationError(errors));
          }

          Right(Box::new(PayPalPaymentMethod::new(payer_details)))
        },
        Err(e) => {
          Left(ValidationResult::ParseError(e.to_string()))
        }
    }
  }
}
