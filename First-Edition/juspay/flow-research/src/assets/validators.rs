  use std::collections::HashMap;
  use either::Either;
  use either::Either::{Left, Right};

  use crate::common_types::*;
  use crate::domain::types::*;
  use crate::domain::extensibility::payment_processor::*;
  use crate::domain::extensibility::payment_method::*;


pub fn validate_payment_method(
  factories: &HashMap<String, Box<dyn IPaymentMethodFactory>>,
  payment_method: &Option<GenericPaymentMethod>)
  -> Either<ValidationResult, Box<dyn IPaymentMethod>> {

    match payment_method {
      Some(pm) => {
        let factory = factories.get(&pm.code);

        match factory {
          Some(f) => {
            f.validate_payment_method(pm)
          },
          None => {
            Left(ValidationResult::Invalid("Invalid payment method".to_string()))
          }
        }
      },
      None => {
        Left(ValidationResult::Missing("Payment method is missing".to_string()))
      }
    }
}

pub fn validate_desired_payment_processors(
  factories: &HashMap<String, Box<dyn IPaymentProcessorFactory>>,
  processors: &Option<Vec<GenericPaymentProcessorDef>>)
  -> Either<ValidationResult, Vec<Box<dyn IPaymentProcessor>>> {

    if let Some(pps) = processors {
        let mut result = Vec::new();

        for pp in pps.iter() {
          let factory = factories.get(&pp.code);

          if let Some(f) = factory {
            let p = f.validate_payment_processor(pp);

            match p {
              Right(p) => {
                result.push(p);
              },
              Left(e) => {
                return Left(e);
              }
            }
          }
        }

        return Right(result)
    }

    Right(Vec::new())
}

pub fn validate_amount(amount: Amount) -> Either<ValidationResult, Amount> {
  if amount > 0 {
    Right(amount)
  } else {
    Left(ValidationResult::Invalid("Invalid amount".to_string()))
  }
}

pub fn validate_currency(
  currencies: &Vec<Currency>,
  currency_json: &String) -> Either<ValidationResult, Currency> {

  let parsed_currency = serde_json::from_str(&currency_json);

  if let Ok(c) = parsed_currency {
    if currencies.contains(&c) {
      Right(c)
    } else {
      Left(ValidationResult::Invalid("Unsupported currency".to_string()))
    }
  } else {
    Left(ValidationResult::Invalid("Invalid currency".to_string()))
  }
}

pub fn validate_confirmation(confirmation: &Option<String>) ->
Either<ValidationResult, Confirmation> {
  if let Some(c) = confirmation {
    if c == "automatic" {
      Right(Confirmation::Automatic)
    } else if c == "manual" {
      Right(Confirmation::Manual)
    } else {
      Left(ValidationResult::Invalid("Invalid confirmation".to_string()))
    }
  } else {
    Right(Confirmation::Manual)
  }
}

pub fn validate_capture_method(capture_method: &Option<String>) ->
Either<ValidationResult, CaptureMethod> {
  if let Some(c) = capture_method {
    if c == "automatic" {
      Right(CaptureMethod::Automatic)
    } else if c == "manual" {
      Right(CaptureMethod::Manual)
    } else {
      Left(ValidationResult::Invalid("Invalid capture method".to_string()))
    }
  } else {
    Right(CaptureMethod::Manual)
  }
}

pub fn validate_description(description: &Option<String>) ->
Either<ValidationResult, String> {
  if let Some(d) = description {
    Right(d.clone())
  } else {
    Right("".to_string())
  }
}

pub fn validate_merchant_auth(auth: &Auth) ->
Either<ValidationResult, Auth> {
  if auth.party_id.len() > 0 && auth.api_key.len() > 0 {
    Right(auth.clone())
  } else {
    Left(ValidationResult::Invalid("Invalid merchant auth".to_string()))
  }
}
