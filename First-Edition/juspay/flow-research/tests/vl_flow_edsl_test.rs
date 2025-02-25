// Demo tests for value-level eDSL for flow construction.

#[cfg(test)]
mod tests {

  use flow_research::common_types::*;
  use flow_research::api::types as api;
  use std::collections::HashMap;


  #[derive(Debug, Clone)]
  pub enum ValidationResult {
    Invalid(String),
    Missing(String),
    ParseError(String),
    ValidationError(Vec<String>),
  }

  // Extensible payment methods infrastructure

  pub trait IPaymentMethod {
    fn code(&self) -> String;
  }

  pub trait IPaymentMethodFactory {
    fn validate_payment_method(&self, payment_method: &api::GenericPaymentMethod)
      -> Result<Box<dyn IPaymentMethod>, ValidationResult>;
  }

  // Specific payment methods

  pub mod ext_pm_card {
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
      fn validate_payment_method(&self, method: &api::GenericPaymentMethod)
        -> Result<Box<dyn IPaymentMethod>, ValidationResult> {

          if method.code != code() {
            return Err(ValidationResult::Invalid("Invalid payment method".to_string()));
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
                return Err(ValidationResult::ValidationError(errors));
              }

              Ok(Box::new(CardPaymentMethod::new(card_details)))
            },
            Err(e) => {
              Err(ValidationResult::ParseError(e.to_string()))
            }
        }
      }
    }
  }

  pub mod ext_paypal_common {
    use serde::{Serialize, Deserialize};

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub enum PayPalProvider {
      PayPal,
      BrainTree,
    }
  }


  pub mod ext_pm_paypal {
    use super::*;
    use serde::{Serialize, Deserialize};

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct PayPalPayerDetails {
      pub payer_id: String,
      pub payer_email: String,
      pub provider: ext_paypal_common::PayPalProvider,
    }

    pub fn code() -> String {
      "paypal".to_string()
    }

    pub struct PayPalPaymentMethod {
      payer_details: PayPalPayerDetails,
    }

    impl PayPalPaymentMethod {
      pub fn new(payer_details: PayPalPayerDetails) -> Self {
        PayPalPaymentMethod {
          payer_details,
        }
      }
    }

    impl IPaymentMethod for PayPalPaymentMethod {
      fn code(&self) -> String {
        code()
      }
    }

    pub struct PayPalPaymentMethodFactory;

    impl IPaymentMethodFactory for PayPalPaymentMethodFactory {
      fn validate_payment_method(&self, method: &api::GenericPaymentMethod)
        -> Result<Box<dyn IPaymentMethod>, ValidationResult> {

          if method.code != code() {
            return Err(ValidationResult::Invalid("Invalid payment method".to_string()));
          }

          let e_payer_details =
            serde_json::from_value::<PayPalPayerDetails>(method.details.clone());

          match e_payer_details {
            Ok(payer_details) => {
              let mut errors = Vec::new();

              // Dummy validation logic

              if payer_details.payer_id.is_empty() {
                errors.push("Invalid payer ID".to_string());
              }

              if payer_details.payer_email.len() == 0 {
                errors.push("Invalid payer email".to_string());
              }

              if errors.len() > 0 {
                return Err(ValidationResult::ValidationError(errors));
              }

              Ok(Box::new(PayPalPaymentMethod::new(payer_details)))
            },
            Err(e) => {
              Err(ValidationResult::ParseError(e.to_string()))
            }
        }
      }
    }
  }

  // Extensible payment processors infrastructure

  pub trait IPaymentProcessor {
    fn code(&self) -> String;
  }

  pub trait IPaymentProcessorFactory {
    fn validate_payment_processor(&self, payment_processor: &api::GenericPaymentProcessor)
      -> Result<Box<dyn IPaymentProcessor>, ValidationResult>;
  }

  // Specific payment processors

  pub mod ext_pp_paypal {
    use super::*;
    use serde::{Serialize, Deserialize};

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct PayPalProcessorDetails {
      pub client_id: String,
      pub client_secret: String,
    }

    pub fn code() -> String {
      "paypal".to_string()
    }

    pub struct PayPalProcessor {
      processor_details: PayPalProcessorDetails,
    }

    impl PayPalProcessor {
      pub fn new(processor_details: PayPalProcessorDetails) -> Self {
        PayPalProcessor {
          processor_details,
        }
      }
    }

    impl IPaymentProcessor for PayPalProcessor {
      fn code(&self) -> String {
        code()
      }
    }

    pub struct PayPalProcessorFactory;

    impl IPaymentProcessorFactory for PayPalProcessorFactory {
      fn validate_payment_processor(&self, processor: &api::GenericPaymentProcessor)
        -> Result<Box<dyn IPaymentProcessor>, ValidationResult> {

          if processor.code != code() {
            return Err(ValidationResult::Invalid("Invalid payment processor".to_string()));
          }

          let e_processor_details =
            serde_json::from_value::<PayPalProcessorDetails>(processor.details.clone());

          match e_processor_details {
            Ok(processor_details) => {
              let mut errors = Vec::new();

              // Dummy validation logic

              if processor_details.client_id.is_empty() {
                errors.push("Invalid client ID".to_string());
              }

              if processor_details.client_secret.len() == 0 {
                errors.push("Invalid client secret".to_string());
              }

              if errors.len() > 0 {
                return Err(ValidationResult::ValidationError(errors));
              }

              Ok(Box::new(PayPalProcessor::new(processor_details)))
            },
            Err(e) => {
              Err(ValidationResult::ParseError(e.to_string()))
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

    let card_details = ext_pm_card::CardDetails {
      card_number: "1234 5678 9012 3456".to_string(),
      card_holder_name: "Alice".to_string(),
      expiry_month: 12,
      expiry_year: 2027,
      cvv: "123".to_string(),
    };

    let card_payment_method = api::GenericPaymentMethod {
      code: "card".to_string(),
      details: serde_json::to_value(card_details).unwrap(),
    };

    let paypal_payment_processor = api::GenericPaymentProcessor {
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

  pub fn validate_payment_method(
    factories: &HashMap<String, Box<dyn IPaymentMethodFactory>>,
    payment_method: &Option<api::GenericPaymentMethod>)
    -> Result<Box<dyn IPaymentMethod>, ValidationResult> {

      match payment_method {
        Some(pm) => {
          let factory = factories.get(&pm.code);

          match factory {
            Some(f) => {
              f.validate_payment_method(pm)
            },
            None => {
              Err(ValidationResult::Invalid("Invalid payment method".to_string()))
            }
          }
        },
        None => {
          Err(ValidationResult::Missing("Payment method is missing".to_string()))
        }
      }
  }

  pub fn validate_payment_processors(
    factories: &HashMap<String, Box<dyn IPaymentProcessorFactory>>,
    processors: &Option<Vec<api::GenericPaymentProcessor>>)
    -> Result<Vec<Box<dyn IPaymentProcessor>>, ValidationResult> {

      if let Some(pps) = processors {
          let mut result = Vec::new();

          for pp in pps.iter() {
            let factory = factories.get(&pp.code);

            if let Some(f) = factory {
              let p = f.validate_payment_processor(pp);

              match p {
                Ok(p) => {
                  result.push(p);
                },
                Err(e) => {
                  return Err(e);
                }
              }
            }
          }

          return Ok(result)
      }

      todo!();
  }


  #[test]
  fn test_payment_methods_creation() {

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

    let pp_result = validate_payment_processors(
      &p_processor_factories,
      &payment_request.connectors);

    assert!(pm_result.is_ok());
    assert!(pp_result.is_ok());

  }

}
