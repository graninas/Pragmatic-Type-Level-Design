use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::extensibility::payment_processor::*;


// Dummy payment processor

#[derive(Clone)]
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
    payment_id: &PaymentId,
    _payment_data: &PaymentData,
    _order_metadata: &OrderMetaData,
  ) -> Result<ThirdPartyPayment, String> {
    Ok(ThirdPartyPayment {
        third_party_payment_id: Some(payment_id.clone()),
      })
  }
}
