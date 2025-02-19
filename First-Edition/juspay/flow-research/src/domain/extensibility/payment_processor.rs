use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;

pub trait IPaymentProcessor {
  fn name(&self) -> &str;
  fn code(&self) -> &str;

  fn process_payment(
      &self,
      customer_profile: &CustomerProfile,
      merchant_profile: &MerchantProfile,
      payment_id: &PaymentId,
      payment_data: &PaymentData,
      order_metadata: &OrderMetaData,
  ) -> Result<ThirdPartyPayment, String>;
}

