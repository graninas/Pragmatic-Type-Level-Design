use serde_json::Value;
use serde_json::json;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;


#[derive(Debug, Serialize, Deserialize, Eq, Hash, PartialEq, Clone)]
pub enum ValueTag {
  // Add more tags here
  Amount,
  Currency,
  PaymentMethod,
  Description,
  CardNumber,
  CardExpiry,
  CardCVV,
}

pub trait IRequestBuilder {
  fn set_value(self: Box<Self>, tag: ValueTag, value: Value) -> Box<dyn IRequestBuilder>;
  fn build_json(&self) -> Value;
  // possibly add build_xml, build_form_data, etc.
}


#[derive(Default)]
pub struct GenericRequestBuilder {
  values: HashMap<ValueTag, Value>,
}

impl GenericRequestBuilder {
  pub fn new() -> Self {
    Self::default()
  }
}

impl IRequestBuilder for GenericRequestBuilder {
  fn set_value(mut self: Box<Self>, tag: ValueTag, value: Value) -> Box<dyn IRequestBuilder> {
    self.values.insert(tag, value);
    self
  }

  fn build_json(&self) -> Value {
    let mut json = json!({});
    for (tag, value) in self.values.clone() {
      match tag {
        ValueTag::Amount => json["amount"] = value,
        ValueTag::Currency => json["currency"] = value,
        ValueTag::PaymentMethod => json["payment_method"] = value,
        ValueTag::Description => json["description"] = value,
        ValueTag::CardNumber => json["card_number"] = value,
        ValueTag::CardExpiry => json["card_expiry"] = value,
        ValueTag::CardCVV => json["card_cvv"] = value,
      }
    }
    json
  }
}

