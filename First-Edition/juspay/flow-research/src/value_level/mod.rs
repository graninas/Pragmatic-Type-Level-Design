use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

// Infrastructure

pub trait ILogger {
    fn log(&self, message: String);
}


// General types

pub type Amount = u32;         // AG: just a demo type
pub type Currency = String;
pub type PaymentMethod = String;

#[derive(Debug, Serialize, Deserialize)]
pub enum Confirmation {
    Manual,
    Automatic,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CaptureMethod {
    Manual,
    Automatic,
}

pub type ApiKey = String;

#[derive(Debug, Serialize, Deserialize)]
pub struct MerchantDetails {
    pub name: String,
    pub email: String,
    pub phone: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MerchantProfile {
    pub merchant_id: String,
    pub merchant_details: MerchantDetails,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Payment {
    pub payment_id: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CustomerDetails {
    pub name: Option<String>,
    pub email: Option<String>,
    pub phone: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CustomerProfile {
    pub customer_id: String,
    pub customer_details: CustomerDetails,
}

pub type CustomerId = String;
pub type MerchantId = String;
pub type OrderId = String;

#[derive(Debug, Serialize, Deserialize)]
pub struct OrderMetaData {
    pub order_id: String,
    pub order_description: String,
    pub encoded_details: Value,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PaymentData {
    pub payment_id: String,
    pub amount: Amount,
    pub currency: Currency,
    pub payment_method: PaymentMethod,
    pub description: Option<String>,
    pub confirmation: Option<Confirmation>,
    pub capture_method: Option<CaptureMethod>,
}


pub mod api {
  use serde::Deserialize;
  use serde::Serialize;

  use crate::value_level::*;

    #[derive(Debug, Serialize, Deserialize)]
    pub struct PaymentRequest {
      payment_id: String,
      amount: Amount,
      currency: Currency,
      payment_method: PaymentMethod,
      description: Option<String>,
      confirmation: Option<Confirmation>,
      capture_method: Option<CaptureMethod>,
      customer_id: Option<CustomerId>,
      customer_details: Option<CustomerDetails>,
      order_metadata: OrderMetaData,
    }
}


// Domain-level services

pub trait ICustomerManager {
    fn create_customer(&self, customer_details: CustomerDetails)
      -> Result<CustomerProfile, String>;
    fn get_customer(&self, customer_id: String)
      -> Result<Option<CustomerProfile>, String>;
}

pub trait IMerchantManager {
    fn create_merchant(&self, merchant_details: MerchantDetails)
      -> Result<MerchantProfile, String>;
    fn get_merchant(&self, merchant_id: MerchantId)
      -> Result<Option<MerchantProfile>, String>;
}

pub trait IPaymentProcessor {
  fn name(&self) -> &str;
  fn code(&self) -> &str;

  fn process_payment(
      &self,
      customer_profile: &CustomerProfile,
      merchant_profile: &MerchantProfile,
      payment_data: &PaymentData,
      order_metadata: &OrderMetaData,
  ) -> Result<Payment, String>;
}

// Template flows

pub trait PaymentCreationFlow {

  fn customer_manager(&self) -> &dyn ICustomerManager;
  fn merchant_manager(&self) -> &dyn IMerchantManager;

  fn get_or_create_customer(
    &self,
    customer_data: Either<CustomerId, CustomerDetails>,
  ) -> Result<CustomerProfile, String> {
    match customer_data {
      Either::Left(customer_id) => {
        let mb_customer = self.customer_manager().get_customer(customer_id);
        match mb_customer {
          Ok(Some(customer_profile)) => Ok(customer_profile),
          Ok(None) => Err("Customer not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(customer_details) => {
        self.customer_manager().create_customer(customer_details)
      }
    }
  }

  fn get_or_create_merchant(
    &self,
    merchant_data: Either<MerchantId, MerchantDetails>,
  ) -> Result<MerchantProfile, String> {
    match merchant_data {
      Either::Left(merchant_id) => {
        let mb_merchant = self.merchant_manager().get_merchant(merchant_id);
        match mb_merchant {
          Ok(Some(merchant_profile)) => Ok(merchant_profile),
          Ok(None) => Err("Merchant not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(merchant_details) => {
        self.merchant_manager().create_merchant(merchant_details)
      }
    }
  }

  fn register_order_metadata(
    &self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<(), String>;

  fn decide_payment_processor(
    &self,
    merchant_profile: &MerchantProfile,
    order_metadata: &OrderMetaData,
  ) -> Result<Box<dyn IPaymentProcessor>, String>;

  fn make_payment_with_payment_processor(
    &self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    customer_profile: &CustomerProfile,
    merchant_profile: &MerchantProfile,
    payment_data: &PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Payment, String>;

  fn register_payment(&self, payment: &Payment) -> Result<(), String>;

  fn execute(&self,
    customer_data: Either<CustomerId, CustomerDetails>,
    merchant_data: Either<MerchantId, MerchantDetails>,
    order_metadata: OrderMetaData,
    payment_data: PaymentData,
  ) -> Result<Payment, String> {
    let customer_profile = self.get_or_create_customer(customer_data)?;
    let merchant_profile = self.get_or_create_merchant(merchant_data)?;

    self.register_order_metadata(
      &customer_profile.customer_id,
      &merchant_profile.merchant_id,
      &order_metadata)?;

    let payment_processor = self.decide_payment_processor(
      &merchant_profile,
      &order_metadata)?;

    let payment = self.make_payment_with_payment_processor(
      &payment_processor,
      &customer_profile,
      &merchant_profile,
      &payment_data,
      &order_metadata)?;

    self.register_payment(&payment)?;

    Ok(payment)
  }
}

// Dummy payment processor

pub struct DummyPaymentProcessor;

impl IPaymentProcessor for DummyPaymentProcessor {
  fn name(&self) -> &str {
    "Dummy Payment Processor"
  }

  fn code(&self) -> &str {
    "dummy"
  }

  fn process_payment(
    &self,
    _customer_profile: &CustomerProfile,
    _merchant_profile: &MerchantProfile,
    _payment_data: &PaymentData,
    _order_metadata: &OrderMetaData,
  ) -> Result<Payment, String> {
    Ok(Payment { payment_id: "dummy_payment_id".to_string() })
  }
}

// Template flows implementation

// Simple payment flow implementation
pub struct SimplePaymentCreationFlow {
  customer_manager: Box<dyn ICustomerManager>,
  merchant_manager: Box<dyn IMerchantManager>,
}

impl SimplePaymentCreationFlow {
  pub fn new(
    customer_manager: Box<dyn ICustomerManager>,
    merchant_manager: Box<dyn IMerchantManager>,
  ) -> Self {
    Self { customer_manager, merchant_manager }
  }
}

impl PaymentCreationFlow for SimplePaymentCreationFlow {

  fn customer_manager(&self) -> &dyn ICustomerManager {
    &*self.customer_manager
  }

  fn merchant_manager(&self) -> &dyn IMerchantManager {
    &*self.merchant_manager
  }

  fn get_or_create_customer(
    &self,
    customer_data: Either<CustomerId, CustomerDetails>,
  ) -> Result<CustomerProfile, String> {
    match customer_data {
      Either::Left(customer_id) => {
        let mb_customer = self.customer_manager.get_customer(customer_id);
        match mb_customer {
          Ok(Some(customer_profile)) => Ok(customer_profile),
          Ok(None) => Err("Customer not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(customer_details) => {
        self.customer_manager.create_customer(customer_details)
      }
    }
  }

  fn get_or_create_merchant(
    &self,
    merchant_data: Either<MerchantId, MerchantDetails>,
  ) -> Result<MerchantProfile, String> {
    match merchant_data {
      Either::Left(merchant_id) => {
        let mb_merchant = self.merchant_manager.get_merchant(merchant_id);
        match mb_merchant {
          Ok(Some(merchant_profile)) => Ok(merchant_profile),
          Ok(None) => Err("Merchant not found".to_string()),
          Err(e) => Err(e),
        }
      },
      Either::Right(merchant_details) => {
        self.merchant_manager.create_merchant(merchant_details)
      }
    }
  }

  fn register_order_metadata(
    &self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<(), String> {
    Ok(())
  }

  fn decide_payment_processor(
    &self,
    merchant_profile: &MerchantProfile,
    order_metadata: &OrderMetaData,
  ) -> Result<Box<dyn IPaymentProcessor>, String> {
    Ok(Box::new(DummyPaymentProcessor))
  }

  fn make_payment_with_payment_processor(
    &self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    customer_profile: &CustomerProfile,
    merchant_profile: &MerchantProfile,
    payment_data: &PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Payment, String> {
    payment_processor.process_payment(customer_profile, merchant_profile, payment_data, order_metadata)
  }

  fn register_payment(&self, payment: &Payment) -> Result<(), String> {
    Ok(())
  }
}


// Flow implementation with logging
pub struct LoggingPaymentCreationFlow {
  inner: Box<dyn PaymentCreationFlow>,
  logger: Box<dyn ILogger>,
}

impl LoggingPaymentCreationFlow {
  pub fn new(inner: Box<dyn PaymentCreationFlow>, logger: Box<dyn ILogger>) -> Self {
    Self { inner, logger }
  }
}

impl PaymentCreationFlow for LoggingPaymentCreationFlow {

  fn customer_manager(&self) -> &dyn ICustomerManager {
    self.inner.customer_manager()
  }

  fn merchant_manager(&self) -> &dyn IMerchantManager {
    self.inner.merchant_manager()
  }

  fn get_or_create_customer(
    &self,
    customer_data: Either<CustomerId, CustomerDetails>,
  ) -> Result<CustomerProfile, String> {
    let customer = self.inner.get_or_create_customer(customer_data)?;
    self.logger.log(format!("Customer created: {:?}", customer));
    Ok(customer)
  }

  fn get_or_create_merchant(
    &self,
    merchant_data: Either<MerchantId, MerchantDetails>,
  ) -> Result<MerchantProfile, String> {
    let merchant = self.inner.get_or_create_merchant(merchant_data)?;
    self.logger.log(format!("Merchant created: {:?}", merchant));
    Ok(merchant)
  }

  fn register_order_metadata(&self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<(), String> {
    self.inner.register_order_metadata(customer_id, merchant_id, order_metadata)
  }

  fn decide_payment_processor(&self,
    merchant_profile: &MerchantProfile,
    order_metadata: &OrderMetaData,
  ) -> Result<Box<dyn IPaymentProcessor>, String> {
    let processor = self.inner.decide_payment_processor(merchant_profile, order_metadata)?;
    self.logger.log(format!("Payment processor selected: {:?}", processor.code()));
    Ok(processor)
  }

  fn make_payment_with_payment_processor(
    &self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    customer_profile: &CustomerProfile,
    merchant_profile: &MerchantProfile,
    payment_data: &PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Payment, String> {
    let payment = self.inner.make_payment_with_payment_processor(
      payment_processor, customer_profile, merchant_profile, payment_data, order_metadata)?;
    self.logger.log(format!("Payment created: {:?}", payment));
    Ok(payment)
  }

  fn register_payment(&self, payment: &Payment) -> Result<(), String> {
    self.inner.register_payment(payment)
  }

  fn execute(&self,
    customer_data: Either<CustomerId, CustomerDetails>,
    merchant_data: Either<MerchantId, MerchantDetails>,
    order_metadata: OrderMetaData,
    payment_data: PaymentData,
  ) -> Result<Payment, String> {
    let result = self.inner.execute(customer_data, merchant_data, order_metadata, payment_data);
    match result {
      Ok(payment) => {
        println!("Payment created: {:?}", payment);
        Ok(payment)
      },
      Err(e) => {
        println!("Payment creation failed: {:?}", e);
        Err(e)
      }
    }
  }
}


