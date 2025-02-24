// Demo tests for value-level eDSL for flow construction.

#[cfg(test)]
mod tests {

  use flow_research::common_types::*;
  use flow_research::api::types as api;
  use std::collections::HashMap;


  #[derive(Debug, Clone)]
  pub enum ValidationError {
    InvalidPaymentMethod,
    ParseError(String),
    ValidationError(Vec<String>),
  }

  pub trait IPaymentMethodFactory {
    fn validate_payment_method(&self, payment_method: &api::GenericPaymentMethod)
      -> Result<Box<dyn IPaymentMethod>, ValidationError>;
  }

  pub trait IPaymentMethod {
    fn code(&self) -> String;
  }


  pub mod ext {
    use super::*;

    use serde::{Serialize, Deserialize};

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct CardDetails {
      pub card_number: String,
      pub card_holder_name: String,
      pub expiry_month: u32,
      pub expiry_year: u32,
      pub cvv: String,
    }

    pub fn card_payment_method_code() -> String {
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
        card_payment_method_code()
      }
    }

    pub struct CardPaymentMethodFactory;

    impl IPaymentMethodFactory for CardPaymentMethodFactory {
      fn validate_payment_method(&self, method: &api::GenericPaymentMethod)
        -> Result<Box<dyn IPaymentMethod>, ValidationError> {

          if method.payment_method != card_payment_method_code() {
            return Err(ValidationError::InvalidPaymentMethod);
          }

          let e_card_details =
            serde_json::from_value::<CardDetails>(method.payment_method_details.clone());

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
                return Err(ValidationError::ValidationError(errors));
              }

              Ok(Box::new(CardPaymentMethod::new(card_details)))
            },
            Err(e) => {
              Err(ValidationError::ParseError(e.to_string()))
            }
        }
      }
    }
  }


  pub fn dummy_payment_request() -> api::PaymentRequest {
    let order_metadata = api::OrderMetaDataExtended {
      order_id: "123".to_string(),
      description: "Order 123".to_string(),
      item: "item".to_string(),
      quantity: 1,
    };

    let customer_details = api::CustomerDetailsExtended {
      name: Some("Alice".to_string()),
      email: Some("abc@cde@fgh".to_string()),
      phone: Some("123 456".to_string()),
      address: Some("123 Main St".to_string()),
      city: Some("Anytown".to_string()),
      state: Some("AS".to_string()),
      country: Some("US".to_string()),
      postal_code: Some("12345".to_string()),
    };

    let card_details = ext::CardDetails {
      card_number: "1234 5678 9012 3456".to_string(),
      card_holder_name: "Alice".to_string(),
      expiry_month: 12,
      expiry_year: 2027,
      cvv: "123".to_string(),
    };

    let card_payment_method = api::GenericPaymentMethod {
      payment_method: "card".to_string(),
      payment_method_details: serde_json::to_value(card_details).unwrap(),
    };

    api::PaymentRequest {
      amount: 100,
      currency: "USD".to_string(),
      payment_method: Some(card_payment_method),
      description: Some("Payment for order 123".to_string()),
      confirmation: Some("automatic".to_string()),
      capture_method: Some("automatic".to_string()),
      customer_id: None,
      customer_details: Some(serde_json::to_value(customer_details).unwrap()),
      order_metadata: Some(serde_json::to_value(order_metadata).unwrap()),
    }
  }


  #[test]
  fn test_payment_methods_creation() {
    let payment_request = dummy_payment_request();

    let card_pm_factory = ext::CardPaymentMethodFactory;
    // let mut payment_methods = HashMap::new();
    // payment_methods.insert(ext::card_payment_method_code(), card_pm_factory);

    let result = card_pm_factory.validate_payment_method(payment_request.payment_method.as_ref().unwrap());


    match result {
      Ok(pm) => {
        assert_eq!(pm.code(), ext::card_payment_method_code());
      },
      Err(e) => {
        panic!("Error: {:?}", e);
      }
    }
  }

}
