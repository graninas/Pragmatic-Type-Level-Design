#[cfg(test)]
mod tests {
  use super::*;

  use std::collections::HashMap;
  use serde_json::Value;
  use either::Left;
  use flow_research::value_level::*;


  // Domain-level infrastructure dummy implementation

  pub struct DummyCustomerManager {
      customer_id_counter: u32,
      customers: HashMap<CustomerId, CustomerProfile>,
  }

  impl DummyCustomerManager {
      pub fn new(customer_id_base: u32) -> Self {
          Self {
              customer_id_counter: customer_id_base,
              customers: HashMap::new(),
          }
      }
  }

  impl ICustomerManager for DummyCustomerManager {
      fn create_customer(
          &mut self,
          customer_details: CustomerDetails,
      ) -> Result<CustomerProfile, String> {
          let customer_id = self.customer_id_counter.to_string();
          self.customer_id_counter += 1;
          let customer_profile = CustomerProfile {
              customer_id: customer_id.clone(),
              customer_details: customer_details,
          };
          self.customers.insert(
            customer_id.clone(),
            customer_profile.clone());
          Ok(customer_profile)
      }

      fn get_customer(
          &mut self,
          customer_id: CustomerId,
      ) -> Result<Option<CustomerProfile>, String> {
          match self.customers.get(&customer_id) {
              Some(customer_profile) => Ok(Some(customer_profile.clone())),
              None => Ok(None),
          }
      }
  }

  pub struct DummyMerchantManager {
      merchant_id_counter: u32,
      merchants: HashMap<MerchantId, MerchantProfile>,
  }

  impl DummyMerchantManager {
      pub fn new(merchant_id_base: u32) -> Self {
          Self {
              merchant_id_counter: merchant_id_base,
              merchants: HashMap::new(),
          }
      }
  }

  impl IMerchantManager for DummyMerchantManager {
      fn create_merchant(
          &self,
          merchant_details: MerchantDetails,
      ) -> Result<MerchantProfile, String> {
          let merchant_id = self.merchant_id_counter.to_string();
          self.merchant_id_counter += 1;
          let merchant_profile = MerchantProfile {
              merchant_id: merchant_id.clone(),
              merchant_details: merchant_details,
          };
          self.merchants.insert(
            merchant_id.clone(),
            merchant_profile.clone());
          Ok(merchant_profile)
      }

      fn get_merchant(
          &self,
          merchant_id: MerchantId,
      ) -> Result<Option<MerchantProfile>, String> {
          match self.merchants.get(&merchant_id) {
              Some(merchant_profile) => Ok(Some(merchant_profile.clone())),
              None => Ok(None),
          }
      }
  }




  #[test]
  fn test_payment_creation_flow() {
    let logger = Box::new(DummyLogger::new());

    let customer_manager = Box::new(DummyCustomerManager::new(1));
    let merchant_manager = Box::new(DummyMerchantManager::new(1));

    let flow = Box::new(SimplePaymentCreationFlow::new(customer_manager, merchant_manager));
    let logging_flow = Box::new(LoggingPaymentCreationFlow::new(flow, logger));

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

    let payment_data = PaymentData {
      amount: 100,
      payment_method: "card".to_string(),
      currency: "INR".to_string(),
      description: Some("Payment for order 123".to_string()),
      confirmation: Some(Confirmation::Automatic),
      capture_method: Some(CaptureMethod::Automatic),
    };

    let result = logging_flow.execute(
      Left(customer_data),
      Left(merchant_data),
      order_metadata,
      payment_data);

    assert!(result.is_ok());
  }
}
