use serde_json::Value;
use serde::{Serialize, Deserialize};


// General types

pub type Amount = u32;         // AG: just a demo type
pub type ApiKey = String;
pub type CustomerId = String;
pub type MerchantId = String;
pub type OrderId = String;
pub type PaymentId = String;
pub type ThirdPartyPaymentId = String;


#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Auth {
  pub party_id: String,
  pub api_key: ApiKey,
}


#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GenericPaymentMethod {
  pub code: String,
  pub details: Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GenericPaymentProcessor {
  pub code: String,
  pub details: Value,
}

#[derive(Debug, Clone)]
pub enum ValidationResult {
  Invalid(String),
  Missing(String),
  ParseError(String),
  ValidationError(Vec<String>),
}

