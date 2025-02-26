use either::Either;

use crate::common_types::*;


// Extensible payment methods infrastructure

pub trait IPaymentMethod {
  fn code(&self) -> String;
}

pub trait IPaymentMethodFactory {
  fn validate_payment_method(&self, payment_method: &GenericPaymentMethod)
    -> Either<ValidationResult, Box<dyn IPaymentMethod>>;
}
