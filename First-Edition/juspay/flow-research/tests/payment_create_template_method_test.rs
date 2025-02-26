// Demo tests for Template Method pattern used for flows.

#[cfg(test)]
mod tests {
  use super::*;

  use std::collections::HashMap;
  use serde_json::Value;
  use either::Left;
  use either::Right;

  use flow_research::common_types::*;
  use flow_research::domain::types::*;
  use flow_research::domain::services::*;
  use flow_research::domain::extensibility::payment_processor::*;
  use flow_research::domain::implementation::dummy_customer_manager::*;
  use flow_research::domain::implementation::dummy_merchant_manager::*;
  use flow_research::assets::flow_templates::simple_payment_create::*;
  use flow_research::assets::flows::simple_payment_create::*;
  use flow_research::assets::payment_processors::dummy::*;
  use flow_research::application::services::ILogger;
  use flow_research::application::dummy_logger::DummyLogger;


  #[test]
  fn test_payment_create_flow() {
    let logger = Box::new(DummyLogger::new());

    let customer_manager = Box::new(DummyCustomerManager::new(1));
    let merchant_manager = Box::new(DummyMerchantManager::new(1));

    let flow = Box::new(SimplePaymentCreateFlow::new(customer_manager, merchant_manager));
    let mut logging_flow = Box::new(LoggingPaymentCreateFlow::new(flow, logger));

    let customer_data = CustomerDetails {
      name: Some("Alice".to_string()),
      email: Some("alice@x.com".to_string()),
      phone: Some("123456".to_string()),
    };

    let merchant_data = MerchantDetails {
      name: "Bob".to_string(),
      email: "bob@y.com".to_string(),
      phone: "654321".to_string(),
    };

    let order_metadata = OrderMetaData {
      order_id: "123".to_string(),
      order_description: "Order 123".to_string(),
      encoded_details: Value::String("encoded_details".to_string()),
    };

    let payment_data = SimplePaymentData {
      amount: 100,
      currency: Currency::USD,
      payment_method: "card".to_string(),
      description: Some("Payment for order 123".to_string()),
      confirmation: Some(Confirmation::Automatic),
      capture_method: Some(CaptureMethod::Automatic),
    };

    let result = logging_flow.execute(
      Right(customer_data),
      Right(merchant_data),
      order_metadata,
      payment_data);

    assert!(result.is_ok());
  }



}
