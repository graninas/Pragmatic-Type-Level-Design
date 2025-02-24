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

// God-like type as it is now in HyperSwitch.
// Some fields decide a flow or its part, some are mutually incoherent.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PaymentRequest {
  pub amount: Amount,
  pub currency: String,
  pub payment_method: Option<String>,             // Optional type: decides the flow or its part
  pub description: Option<String>,
  pub confirmation: Option<String>,               // Optional type: decides the flow or its part
  pub capture_method: Option<String>,             // Optional type: decides the flow or its part
  pub customer_id: Option<String>,                // Two inconherent fields:
  pub customer_details: Option<Value>,            // - both can be None (what to do in this case?)
                                                  // - both can be Some (what to do in this case?)
  pub order_metadata: Option<Value>,              // Optional type: what to do if None?
}
