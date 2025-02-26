use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;
use crate::domain::services::*;


pub trait GenericPaymentCreateFlowTemplate {

  type PaymentData;
  type PaymentResult;

  fn merchant_manager(&mut self) -> &mut dyn IMerchantManager;

  // Helper methods

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

  fn get_or_create_payment_id(
    &mut self,
    merchant_id: &MerchantId,
    payment_data: &Self::PaymentData,
  ) -> Result<PaymentId, String>;

  fn register_payment_data(
    &mut self,
    payment_id: &PaymentId,
    merchant_id: &MerchantId,
    payment_data: &Self::PaymentData,
  ) -> Result<(), String>;

  fn decide_payment_processor(
    &mut self,
    merchant_profile: &MerchantProfile,
    payment_data: &Self::PaymentData,
  ) -> Result<Box<dyn IPaymentProcessor>, String>;

  fn make_payment_with_payment_processor(
    &mut self,
    payment_processor: &Box<dyn IPaymentProcessor>,
    merchant_profile: &MerchantProfile,
    payment_id: &PaymentId,
    payment_data: &Self::PaymentData,
  ) -> Result<Self::PaymentResult, String>;

  fn register_payment(&mut self, payment: &Self::PaymentResult) -> Result<(), String>;

  // Template method itself

  fn execute(&mut self,
    payment_data: &Self::PaymentData,
    merchant_auth: &Auth
  ) -> Result<Self::PaymentResult, String> {

    let merchant_id = merchant_auth.party_id.clone();

    let merchant_profile = self.get_merchant(merchant_id.clone())?
      .ok_or("Merchant not found".to_string())?;

    let payment_id = self.get_or_create_payment_id(
      &merchant_id,
      &payment_data)?;

    self.register_payment_data(
      &payment_id,
      &merchant_id,
      &payment_data)?;

    let payment_processor = self.decide_payment_processor(
      &merchant_profile,
      &payment_data)?;

    let payment = self.make_payment_with_payment_processor(
      &payment_processor,
      &merchant_profile,
      &payment_id,
      payment_data)?;

    self.register_payment(&payment)?;

    Ok(payment)

  }
}
