use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::services::*;


pub trait SimplePaymentCreateFlowTemplate {

  fn customer_manager(&mut self) -> &mut dyn ICustomerManager;
  fn merchant_manager(&mut self) -> &mut dyn IMerchantManager;

  // Helper methods

  fn get_customer(&mut self, customer_id: CustomerId
  ) -> Result<Option<CustomerProfile>, String> {
    let manager: &dyn ICustomerManager = self.customer_manager();
    manager.get_customer(customer_id)
  }

  fn create_customer(&mut self, customer_details: CustomerDetails
  ) -> Result<CustomerProfile, String> {
    let manager: &mut dyn ICustomerManager = self.customer_manager();
    manager.create_customer(customer_details)
  }

  fn get_merchant(&mut self, merchant_id: MerchantId
  ) -> Result<Option<MerchantProfile>, String> {
    let manager: &dyn IMerchantManager = self.merchant_manager();
    manager.get_merchant(merchant_id)
  }

  fn create_merchant(&mut self, merchant_details: MerchantDetails
  ) -> Result<MerchantProfile, String> {
    let manager: &mut dyn IMerchantManager = self.merchant_manager();
    manager.create_merchant(merchant_details)
  }

  // Actual flow methods

  fn get_or_create_customer(
    &mut self,
    customer_data: Either<CustomerId, CustomerDetails>,
  ) -> Result<CustomerProfile, String> {
    match customer_data {
      Either::Left(customer_id) => {
        let mb_customer = self.get_customer(customer_id);
        match mb_customer {
          Ok(Some(customer_profile)) => Ok(customer_profile),
          Ok(None) => Err("Customer not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(customer_details) => {
        self.create_customer(customer_details)
      }
    }
  }

  fn get_or_create_merchant(
    &mut self,
    merchant_data: Either<MerchantId, MerchantDetails>,
  ) -> Result<MerchantProfile, String> {
    match merchant_data {
      Either::Left(merchant_id) => {
        let mb_merchant = self.get_merchant(merchant_id);
        match mb_merchant {
          Ok(Some(merchant_profile)) => Ok(merchant_profile),
          Ok(None) => Err("Merchant not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(merchant_details) => {
        self.create_merchant(merchant_details)
      }
    }
  }

  fn get_or_create_payment_id(
    &mut self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<PaymentId, String>;

  fn register_order_metadata(
    &mut self,
    payment_id: &PaymentId,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<(), String>;

  fn decide_payment_processor(
    &mut self,
    merchant_profile: &MerchantProfile,
    payment_data: &PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Box<dyn IPaymentProcessor>, String>;

  fn make_payment_with_payment_processor(
    &mut self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    customer_profile: &CustomerProfile,
    merchant_profile: &MerchantProfile,
    payment_id: &PaymentId,
    payment_data: &PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Payment, String>;

  fn register_payment(&mut self, payment: &Payment) -> Result<(), String>;

  // Template method itself

  fn execute(&mut self,
    customer_data: Either<CustomerId, CustomerDetails>,
    merchant_data: Either<MerchantId, MerchantDetails>,
    order_metadata: OrderMetaData,
    payment_data: PaymentData,
  ) -> Result<Payment, String> {
    let customer_profile = self.get_or_create_customer(customer_data)?;
    let merchant_profile = self.get_or_create_merchant(merchant_data)?;

    let payment_id = self.get_or_create_payment_id(
      &customer_profile.customer_id,
      &merchant_profile.merchant_id,
      &order_metadata)?;

    self.register_order_metadata(
      &payment_id,
      &customer_profile.customer_id,
      &merchant_profile.merchant_id,
      &order_metadata)?;

    let payment_processor = self.decide_payment_processor(
      &merchant_profile,
      &payment_data,
      &order_metadata)?;

    let payment = self.make_payment_with_payment_processor(
      &payment_processor,
      &customer_profile,
      &merchant_profile,
      &payment_id,
      &payment_data,
      &order_metadata)?;

    self.register_payment(&payment)?;

    Ok(payment)
  }
}
