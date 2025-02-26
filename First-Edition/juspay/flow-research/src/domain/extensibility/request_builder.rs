use serde_json::Value;
use serde_json::json;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;


#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
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
  type Request;

  fn set_value(&mut self, tag: ValueTag, value: Value) -> Self;
  fn get_map(&self) -> &HashMap<ValueTag, Value>;
  fn build(&self) -> Self::Request;
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
  type Request = Value;

  fn set_value(&mut self, tag: ValueTag, value: Value) -> Self {
    self.values.insert(tag, value);
    self
  }

  fn get_map(&self) -> &HashMap<ValueTag, Value> {
    &self.values
  }

  fn build(&self) -> Self::Request {
    self.build_json()
  }

  fn build_json(&self) -> Value {
    let mut json = json!({});
    for (tag, value) in self.values {
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
