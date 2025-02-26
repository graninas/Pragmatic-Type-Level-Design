use crate::common_types::*;
use crate::domain::extensibility::payment_method::*;

use either::Either;
use either::Either::{Left, Right};
use serde::{Serialize, Deserialize};


// Card payment method

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CardDetails {
  pub card_number: String,
  pub card_holder_name: String,
  pub expiry_month: u32,
  pub expiry_year: u32,
  pub cvv: String,
}

pub fn code() -> String {
  "card".to_string()
}

pub struct CardPaymentMethod {
  card_details: CardDetails,
}

impl CardPaymentMethod {
  pub fn new(card_details: CardDetails) -> Self {
    CardPaymentMethod {
      card_details,
    }
  }
}

impl IPaymentMethod for CardPaymentMethod {
  fn code(&self) -> String {
    code()
  }
}

pub struct CardPaymentMethodFactory;

impl IPaymentMethodFactory for CardPaymentMethodFactory {
  fn validate_payment_method(&self, method: &GenericPaymentMethod)
    -> Either<ValidationResult, Box<dyn IPaymentMethod>> {

      if method.code != code() {
        return Left(ValidationResult::Invalid("Invalid payment method".to_string()));
      }

      let e_card_details =
        serde_json::from_value::<CardDetails>(method.details.clone());

      match e_card_details {
        Ok(card_details) => {
          let mut errors = Vec::new();

          // Dummy validation logic

          if card_details.card_number.is_empty() {
            errors.push("Invalid card number".to_string());
          }

          if card_details.card_holder_name.len() == 0 {
            errors.push("Invalid card holder name".to_string());
          }

          if card_details.expiry_month < 1 || card_details.expiry_month > 12 {
            errors.push("Invalid expiry month".to_string());
          }

          if card_details.expiry_year < 2025 {
            errors.push("Invalid expiry year".to_string());
          }

          if card_details.cvv.len() != 3 {
            errors.push("Invalid CVV".to_string());
          }

          if errors.len() > 0 {
            return Left(ValidationResult::ValidationError(errors));
          }

          Right(Box::new(CardPaymentMethod::new(card_details)))
        },
        Err(e) => {
          Left(ValidationResult::ParseError(e.to_string()))
        }
    }
  }
}
