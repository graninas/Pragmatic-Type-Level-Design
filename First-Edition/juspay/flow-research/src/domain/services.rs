use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;
use crate::domain::types::*;


// Domain-level services

pub trait ICustomerManager
{
    fn create_customer(&mut self, customer_details: CustomerDetails)
      -> Result<CustomerProfile, String>;
    fn get_customer(&self, customer_id: String)
      -> Result<Option<CustomerProfile>, String>;
}

pub trait IMerchantManager
{
    fn create_merchant(&mut self, merchant_details: MerchantDetails)
      -> Result<MerchantProfile, String>;
    fn get_merchant(&self, merchant_id: MerchantId)
      -> Result<Option<MerchantProfile>, String>;
}
