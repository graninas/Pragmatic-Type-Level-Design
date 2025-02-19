use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};


// General types

pub type Amount = u32;         // AG: just a demo type
pub type RawPaymentMethod = String;
pub type ApiKey = String;
pub type CustomerId = String;
pub type MerchantId = String;
pub type OrderId = String;
pub type PaymentId = String;
pub type ThirdPartyPaymentId = String;

