// Demo tests for value-level eDSL for flow construction.

#[cfg(test)]
mod tests {

  use flow_research::common_types::*;
  use flow_research::api::types as api;
  use std::collections::HashMap;

  pub trait PaymentMethodFactory {
    fn payment_method_code(&self) -> String;
    fn validate_payment_method(&self, payment_method: &api::GenericPaymentMethod) -> Result<(), String>;
  }

  pub mod ext {

    use serde::{Serialize, Deserialize};

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct CardDetails {
      pub card_number: String,
      pub card_holder_name: String,
      pub expiry_month: u32,
      pub expiry_year: u32,
      pub cvv: String,
    }

    pub struct CardPaymentMethod;

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
      expiry_year: 2022,
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
  fn test_validation() {
    let payment_request = dummy_payment_request();

    // let payment_method_factories = vec![ext::card_payment_method_factory()];

    // let mut payment_methods = HashMap::new();
    // for factory in payment_method_factories {
    //   payment_methods.insert(factory.payment_method_code(), factory);
    // }



  }

}
