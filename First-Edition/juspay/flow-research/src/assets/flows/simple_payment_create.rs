use either::Either;
use either::Either::{Left, Right};
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::services::*;
use crate::assets::flow_templates::simple_payment_create::*;
use crate::assets::payment_processors::dummy::*;
use crate::application::services::ILogger;


// Simple payment flow implementation

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct SimplePaymentData {
    pub amount: Amount,
    pub currency: Currency,
    pub payment_method: String,
    pub description: Option<String>,
    pub confirmation: Option<Confirmation>,
    pub capture_method: Option<CaptureMethod>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct SimplePaymentResult {
    pub payment_id: PaymentId,
    pub third_party_payment: ThirdPartyPayment,
    pub payment_data: SimplePaymentData,
}


pub struct SimplePaymentCreateFlow {
  customer_manager: Box<dyn ICustomerManager>,
  merchant_manager: Box<dyn IMerchantManager>,
}

impl SimplePaymentCreateFlow {
  pub fn new(
    customer_manager: Box<dyn ICustomerManager>,
    merchant_manager: Box<dyn IMerchantManager>,
  ) -> Self {
    Self { customer_manager, merchant_manager }
  }
}

impl SimplePaymentCreateFlowTemplate for SimplePaymentCreateFlow {

  type PaymentData = SimplePaymentData;
  type PaymentResult = SimplePaymentResult;

  fn customer_manager(&mut self) -> &mut dyn ICustomerManager {
    &mut *self.customer_manager
  }

  fn merchant_manager(&mut self) -> &mut dyn IMerchantManager {
    &mut *self.merchant_manager
  }

  fn get_or_create_customer(
    &mut self,
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
    &mut self,
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

  fn get_or_create_payment_id(
    &mut self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<PaymentId, String> {
    Ok(format!("{}-{}-{}",
        customer_id,
        merchant_id,
        order_metadata.order_id))
  }

  fn register_order_metadata(
    &mut self,
    payment_id: &PaymentId,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<(), String> {
    Ok(())
  }

  fn decide_payment_processor(
    &mut self,
    merchant_profile: &MerchantProfile,
    payment_data: &Self::PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Box<dyn IPaymentProcessor>, String> {
    Ok(Box::new(DummyPaymentProcessor::new(DummyPaymentProcessorDetails)))
  }

  fn make_payment_with_payment_processor(
    &mut self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    customer_profile: &CustomerProfile,
    merchant_profile: &MerchantProfile,
    payment_id: &PaymentId,
    payment_data: &Self::PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Self::PaymentResult, String> {
    let third_party_payment = payment_processor.process_payment(
      customer_profile,
      merchant_profile,
      payment_id,
      payment_data,
      order_metadata)?;

    Ok(SimplePaymentResult {
      payment_id: payment_id.clone(),
      third_party_payment,
      payment_data: payment_data.clone(),
    })
  }

  fn register_payment(&mut self, payment: &Self::PaymentResult) -> Result<(), String> {
    Ok(())
  }
}


// Flow implementation with logging
pub struct LoggingPaymentCreateFlow {
  inner: Box<dyn SimplePaymentCreateFlowTemplate
    <PaymentData=SimplePaymentData, PaymentResult=SimplePaymentResult>>,
  logger: Box<dyn ILogger>,
}

impl LoggingPaymentCreateFlow {
  pub fn new(inner: Box<dyn SimplePaymentCreateFlowTemplate
                        <PaymentData=SimplePaymentData, PaymentResult=SimplePaymentResult>>,
            logger: Box<dyn ILogger>) -> Self {
    Self { inner, logger }
  }
}

impl SimplePaymentCreateFlowTemplate for LoggingPaymentCreateFlow {

  type PaymentData = SimplePaymentData;
  type PaymentResult = SimplePaymentResult;

  fn customer_manager(&mut self) -> &mut dyn ICustomerManager {
    self.inner.customer_manager()
  }

  fn merchant_manager(&mut self) -> &mut dyn IMerchantManager {
    self.inner.merchant_manager()
  }

  fn get_or_create_customer(
    &mut self,
    customer_data: Either<CustomerId, CustomerDetails>,
  ) -> Result<CustomerProfile, String> {
    let customer = self.inner.get_or_create_customer(customer_data)?;
    self.logger.log(format!("Customer created: {:?}", customer));
    Ok(customer)
  }

  fn get_or_create_merchant(
    &mut self,
    merchant_data: Either<MerchantId, MerchantDetails>,
  ) -> Result<MerchantProfile, String> {
    let merchant = self.inner.get_or_create_merchant(merchant_data)?;
    self.logger.log(format!("Merchant created: {:?}", merchant));
    Ok(merchant)
  }

  fn get_or_create_payment_id(
    &mut self,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<PaymentId, String> {
    self.inner.get_or_create_payment_id(
      customer_id,
      merchant_id,
      order_metadata)
  }

  fn register_order_metadata(
    &mut self,
    payment_id: &PaymentId,
    customer_id: &CustomerId,
    merchant_id: &MerchantId,
    order_metadata: &OrderMetaData,
  ) -> Result<(), String> {
    self.inner.register_order_metadata(
      payment_id,
      customer_id,
      merchant_id,
      order_metadata)
  }

  fn decide_payment_processor(
    &mut self,
    merchant_profile: &MerchantProfile,
    payment_data: &Self::PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Box<dyn IPaymentProcessor>, String> {
    let processor = self.inner.decide_payment_processor(
      merchant_profile,
      payment_data,
      order_metadata)?;
    self.logger.log(format!("Payment processor selected: {:?}", processor.code()));
    Ok(processor)
  }

  fn make_payment_with_payment_processor(
    &mut self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    customer_profile: &CustomerProfile,
    merchant_profile: &MerchantProfile,
    payment_id: &PaymentId,
    payment_data: &Self::PaymentData,
    order_metadata: &OrderMetaData,
  ) -> Result<Self::PaymentResult, String> {
    let payment = self.inner.make_payment_with_payment_processor(
      payment_processor,
      customer_profile,
      merchant_profile,
      payment_id,
      payment_data,
      order_metadata)?;
    self.logger.log(format!("Payment created: {:?}", payment));
    Ok(payment)
  }

  fn register_payment(&mut self, payment: &Self::PaymentResult) -> Result<(), String> {
    self.inner.register_payment(payment)
  }

  fn execute(
    &mut self,
    customer_data: Either<CustomerId, CustomerDetails>,
    merchant_data: Either<MerchantId, MerchantDetails>,
    order_metadata: OrderMetaData,
    payment_data: Self::PaymentData,
  ) -> Result<Self::PaymentResult, String> {
    let result = self.inner.execute(
      customer_data,
      merchant_data,
      order_metadata,
      payment_data);
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


