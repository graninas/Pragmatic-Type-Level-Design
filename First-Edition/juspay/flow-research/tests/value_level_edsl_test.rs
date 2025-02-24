// Demo tests for Template Method pattern used for flows.

#[cfg(test)]
mod tests {

  use flow_research::common_types::*;
  use flow_research::api::types as api;


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

    api::PaymentRequest {
      amount: 100,
      currency: "USD".to_string(),
      payment_method: Some("card".to_string()),
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




  }

}
