use std::collections::HashMap;

use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::services::*;


// Domain-level infrastructure dummy implementation

pub struct DummyCustomerManager {
    customer_id_counter: u32,
    customers: HashMap<CustomerId, CustomerProfile>,
}

impl DummyCustomerManager {
    pub fn new(customer_id_base: u32) -> Self {
        Self {
            customer_id_counter: customer_id_base,
            customers: HashMap::new(),
        }
    }
}

impl ICustomerManager for DummyCustomerManager {
    fn create_customer(
        &mut self,
        customer_details: CustomerDetails,
    ) -> Result<CustomerProfile, String> {
        let customer_id = self.customer_id_counter.to_string();
        self.customer_id_counter += 1;
        let customer_profile = CustomerProfile {
            customer_id: customer_id.clone(),
            customer_details: customer_details,
        };
        self.customers.insert(
          customer_id.clone(),
          customer_profile.clone());
        Ok(customer_profile)
    }

    fn get_customer(
        &self,
        customer_id: CustomerId,
    ) -> Result<Option<CustomerProfile>, String> {
        match self.customers.get(&customer_id) {
            Some(customer_profile) => Ok(Some(customer_profile.clone())),
            None => Ok(None),
        }
    }
}
