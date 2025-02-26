use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};
use strum_macros::EnumString;

use crate::common_types::*;


#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub enum Currency {
  Crypto(String),
  USD,
  EUR,
  GBP,
  // Add more currencies here
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone, EnumString)]
pub enum Confirmation {
    Manual,
    Automatic,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone, EnumString)]
pub enum CaptureMethod {
    Manual,
    Automatic,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct MerchantDetails {
    pub name: String,
    pub email: String,
    pub phone: String,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct MerchantProfile {
    pub merchant_id: String,
    pub merchant_details: MerchantDetails,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct CustomerDetails {
    pub name: Option<String>,
    pub email: Option<String>,
    pub phone: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct CustomerProfile {
    pub customer_id: String,
    pub customer_details: CustomerDetails,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct OrderMetaData {
    pub order_id: String,
    pub order_description: String,
    pub encoded_details: Value,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub struct ThirdPartyPayment {
    pub third_party_payment_id: Option<ThirdPartyPaymentId>,
}

