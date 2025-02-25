use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;


// Extensible payment methods infrastructure

pub trait IPaymentMethod {
  fn code(&self) -> String;
}

pub trait IPaymentMethodFactory {
  fn validate_payment_method(&self, payment_method: &GenericPaymentMethod)
    -> Either<ValidationResult, Box<dyn IPaymentMethod>>;
}
