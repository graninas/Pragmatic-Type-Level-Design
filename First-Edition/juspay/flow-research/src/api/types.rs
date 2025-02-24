use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;


// Details about an order
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct OrderMetaDataExtended {
  pub order_id: String,
  pub description: String,
  pub item: String,
  pub quantity: u32,
}

// Details about a customer
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CustomerDetailsExtended {
  pub name: Option<String>,
  pub email: Option<String>,
  pub phone: Option<String>,
  pub address: Option<String>,
  pub city: Option<String>,
  pub state: Option<String>,
  pub country: Option<String>,
  pub postal_code: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GenericPaymentMethod {
  pub payment_method: String,
  pub payment_method_details: Value,
}

// God-like type as it is now in HyperSwitch.
// Some fields decide a flow or its part, some are mutually incoherent.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PaymentRequest {
  pub amount: Amount,
  pub currency: String,
  // Payment method. Optional type: decides the flow or its part.
  // Contains payment method and its details
  pub payment_method: Option<GenericPaymentMethod>,
  pub description: Option<String>,
  // Confirmation. Optional type: decides the flow or its part
  pub confirmation: Option<String>,
  // Capture method. Optional type: decides the flow or its part
  pub capture_method: Option<String>,
  // Two possibly inconherent fields:
  // - both can be None (what to do in this case?)
  // - both can be Some (what to do in this case?)
  pub customer_id: Option<String>,
  pub customer_details: Option<Value>,
  // Optional type: what to do if None?
  pub order_metadata: Option<Value>,
}
