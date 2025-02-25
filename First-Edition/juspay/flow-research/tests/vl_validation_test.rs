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
  use flow_research::assets::payment_processors::dummy as ext_pp_dummy;
  use flow_research::assets::payment_processors::paypal as ext_pp_paypal;
  use flow_research::assets::payment_methods::card as ext_pm_card;
  use flow_research::assets::payment_methods::paypal as ext_pm_paypal;
  use flow_research::assets::flows::generic_payment_create::* as gen_flow;
  use flow_research::assets::flows::normal_payment_create::* as flow1;


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

    let paypal_payment_processor = GenericPaymentProcessor {
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

  pub fn validate_payment_method(
    factories: &HashMap<String, Box<dyn IPaymentMethodFactory>>,
    payment_method: &Option<api::GenericPaymentMethod>)
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
    processors: &Option<Vec<GenericPaymentProcessor>>)
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

  pub fn validate_amount(amount: &Amount) -> Either<ValidationResult, Amount> {
    if amount.greater(0) {
      Right(amount)
    } else {
      Left(ValidationResult::Invalid("Invalid amount".to_string()))
    }
  }

  pub fn validate_currency(
    currencies: &Vec<Currency>,
    currency: String) -> Either<ValidationResult, Currency> {
      for c in currencies.iter() {
        match c {
          Currency::Crypto(code) => {
            if code == &currency {
              return Right(c.clone());
            }
          },
          _ => {
            if c.to_string() == currency {
              return Right(c.clone());
            }
          }
        }
      }

      Left(ValidationResult::Invalid("Invalid currency".to_string()))
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
      Right(Confirmation::Manuals)
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

    let amount = validate_amount(&payment_request.amount);
    let currency = validate_currency(
      &currencies,
      &payment_request.currency);
    let description = validate_description(&payment_request.description);
    let confirmation = validate_confirmation(&payment_request.confirmation);
    let capture_method = validate_capture_method(&payment_request.capture_method);
    let merchant_auth = validate_merchant_auth(&raw_merchant_auth);

    // Flow selection, construction, and configuring.
    // TODO: introduce a value-level eDSL
    // or reuse the type-level eDSL for flows construction

    let flow_constructor = |
      logger,
      customer_manager,
      merchant_manager| {

      let mut flow_constructors = Vec::new();

      let mut flow1_data = flow1::dummy_normal_flow_payment_data();
      let mut flow1_merchant_auth;

      let flow1_conditions = vec![
        mandatory!(amount, flow1_data.amount),
        mandatory!(currency, flow1_data.currency),
        // mandatory!(payment_method, flow1_data.payment_method),
        optional!(description, "", flow1_data.description),
        optional!(confirmation, Confirmation::Manual, flow1_data.confirmation),
        optional!(capture_method, CaptureMethod::Manual, flow1_data.capture_method),
        mandatory!(merchant_auth, flow1_merchant_auth),
        // todo: payment method should be instant
      ];

      flow_constructors.push(
        (flow1_conditions, || {
          let flow = Box::new(NormalPaymentCreateFlow::new(customer_manager, merchant_manager))



        })
      );





      Box::new(SimplePaymentCreateFlow::new(customer_manager, merchant_manager))
    };




    let customer_manager = Box::new(DummyCustomerManager::new(1));
    let merchant_manager = Box::new(DummyMerchantManager::new(1));
    let logger = Box::new(DummyLogger::new());

    let flow = flow_constructor(logger, customer_manager, merchant_manager);
    // let mut logging_flow = Box::new(LoggingPaymentCreateFlow::new(flow, logger));

    // let result = logging_flow.execute(
    //   Right(customer_data),
    //   Right(merchant_data),
    //   order_metadata,
    //   payment_data);

    assert!(result.is_ok());



  }

}
