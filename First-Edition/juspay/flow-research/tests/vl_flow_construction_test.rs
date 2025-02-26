// Demo tests for value-level eDSL for flow construction.

#[cfg(test)]
mod tests {

  use std::collections::HashMap;
  use either::Either;
  use either::Either::{Left, Right};

  use flow_research::common_types::*;
  use flow_research::api::types as api;
  use flow_research::domain::types::*;
  use flow_research::domain::extensibility::payment_processor::*;
  use flow_research::domain::extensibility::payment_method::*;
  use flow_research::domain::services::ICustomerManager;
  use flow_research::domain::services::IMerchantManager;
  use flow_research::domain::implementation::dummy_customer_manager::*;
  use flow_research::domain::implementation::dummy_merchant_manager::*;
  use flow_research::application::services::ILogger;
  use flow_research::application::dummy_logger::DummyLogger;
  use flow_research::assets::payment_processors::dummy as ext_pp_dummy;
  use flow_research::assets::payment_processors::paypal as ext_pp_paypal;
  use flow_research::assets::payment_methods::card as ext_pm_card;
  use flow_research::assets::payment_methods::paypal as ext_pm_paypal;
  use flow_research::assets::flow_templates::generic_payment_create as gen_flow;
  use flow_research::assets::flows::normal_payment_create as flow1;
  use flow_research::assets::validators;
  use flow_research::crates::validation::macro_edsl::*;


  // Specific payment processors

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

    let card_details = ext_pm_card::CardDetails {
      card_number: "1234 5678 9012 3456".to_string(),
      card_holder_name: "Alice".to_string(),
      expiry_month: 12,
      expiry_year: 2027,
      cvv: "123".to_string(),
    };

    let card_payment_method = GenericPaymentMethod {
      code: "card".to_string(),
      details: serde_json::to_value(card_details).unwrap(),
    };

    let paypal_payment_processor = GenericPaymentProcessorDef {
      code: "paypal".to_string(),
      details: serde_json::to_value(ext_pp_paypal::PayPalProcessorDetails {
        client_id: "123".to_string(),
        client_secret: "456".to_string(),
      }).unwrap(),
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
      connectors: Some(vec![paypal_payment_processor]),
    }
  }

  pub fn dummy_merchant_auth() -> Auth {
    Auth {
      party_id: "123".to_string(),
      api_key: "456".to_string(),
    }
  }




  #[test]
  fn test_validation() {

    // Supported payment methods
    let mut pm_factories: HashMap<String, Box<dyn IPaymentMethodFactory>> = HashMap::new();
    pm_factories.insert(ext_pm_card::code(), Box::new(ext_pm_card::CardPaymentMethodFactory));
    pm_factories.insert(ext_pm_paypal::code(), Box::new(ext_pm_paypal::PayPalPaymentMethodFactory));

    // Supported payment processors
    let mut p_processor_factories: HashMap<String, Box<dyn IPaymentProcessorFactory>> = HashMap::new();
    p_processor_factories.insert(ext_pp_paypal::code(), Box::new(ext_pp_paypal::PayPalProcessorFactory));

    // Parsing and validating a payment request

    let payment_request = dummy_payment_request();

    let pm_result = validate_payment_method(
      &pm_factories,
      &payment_request.payment_method);

    let pp_result = validate_desired_payment_processors(
      &p_processor_factories,
      &payment_request.connectors);

    assert!(pm_result.is_right());
    assert!(pp_result.is_right());

    // Use pm_result and pp_result for choosing and configuring a flow

  }

  #[test]
  fn test_payment_create_handler() {

    // Supported payment methods
    let mut pm_factories: HashMap<String, Box<dyn IPaymentMethodFactory>> = HashMap::new();
    pm_factories.insert(ext_pm_card::code(), Box::new(ext_pm_card::CardPaymentMethodFactory));
    pm_factories.insert(ext_pm_paypal::code(), Box::new(ext_pm_paypal::PayPalPaymentMethodFactory));

    // Supported payment processors
    let mut p_processor_factories: HashMap<String, Box<dyn IPaymentProcessorFactory>> = HashMap::new();
    p_processor_factories.insert(ext_pp_paypal::code(), Box::new(ext_pp_paypal::PayPalProcessorFactory));

    // Supported currencies
    let currencies = vec![
      Currency::Crypto("BTC".to_string()),
      Currency::USD,
      Currency::EUR,
      Currency::GBP,
    ];


    // Parsing and validating a payment request

    let payment_request = dummy_payment_request();
    let raw_merchant_auth = dummy_merchant_auth();

    // TODO: introduce validation eDSL or reuse the type-level eDSL for flows construction

    let payment_method = validate_payment_method(
      &pm_factories,
      &payment_request.payment_method);

    let payment_processor = validate_desired_payment_processors(
      &p_processor_factories,
      &payment_request.connectors);

    let amount = validate_amount(payment_request.amount);
    let currency = validate_currency(&currencies, &payment_request.currency);
    let description = validate_description(&payment_request.description);
    let confirmation = validate_confirmation(&payment_request.confirmation);
    let capture_method = validate_capture_method(&payment_request.capture_method);
    let merchant_auth = validate_merchant_auth(&raw_merchant_auth);

    // Flow selection, construction, and configuring.
    // TODO: introduce a value-level eDSL
    // or reuse the type-level eDSL for flows construction

    let flow_constructor = || {

      let mut flow1_data = flow1::dummy_normal_flow_payment_data();
      let mut flow1_merchant_auth = dummy_auth();

      let flow1_parameters: Vec<Box<dyn FnMut() -> Result<(), String>>> = vec![
        mandatory!(amount, flow1_data.amount),
        mandatory!(currency, flow1_data.currency),
        // mandatory!(payment_method, flow1_data.payment_method),
        optional!(description, "".to_string(), flow1_data.description),
        optional!(confirmation, Confirmation::Manual, flow1_data.confirmation),
        optional!(capture_method, CaptureMethod::Manual, flow1_data.capture_method),
        mandatory!(merchant_auth, flow1_merchant_auth),
      ];

      // todo: payment method should be instant

      let mut flow1_decided = true;
      for mut param in flow1_parameters {
        if let Err(e) = param() {
          flow1_decided = false;
          break;
        }
      }

      let flow: Box<dyn FnMut(Box<dyn ICustomerManager>, Box<dyn IMerchantManager>)
        -> flow1::NormalPaymentCreateFlowResult> =
      if flow1_decided {
          Box::new(move |customer_manager, merchant_manager| {
              let mut flow1 = flow1::NormalPaymentCreateFlow::new(
                  merchant_manager
              );
              flow1.execute(&flow1_data.clone(), &flow1_merchant_auth.clone())
          })
      } else {
          Box::new(move |_, _| {
              Err("Flow 1 parameters are not valid".to_string())
          })
      };

      flow

    };

    let customer_manager = Box::new(DummyCustomerManager::new(1));
    let merchant_manager = Box::new(DummyMerchantManager::new(1));
    let logger = Box::new(DummyLogger::new());

    let mut flow_f = flow_constructor();
    let result = flow_f(customer_manager, merchant_manager);

    assert!(result.is_ok());

  }

}

