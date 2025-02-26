use std::collections::HashMap;

use crate::common_types::*;
use crate::domain::types::*;
use crate::domain::services::*;


// Domain-level infrastructure dummy implementation

pub struct DummyMerchantManager {
    merchant_id_counter: u32,
    merchants: HashMap<MerchantId, MerchantProfile>,
}

impl DummyMerchantManager {
    pub fn new(merchant_id_base: u32) -> Self {
        Self {
            merchant_id_counter: merchant_id_base,
            merchants: HashMap::new(),
        }
    }
}

impl IMerchantManager for DummyMerchantManager {
    fn create_merchant(
        &mut self,
        merchant_details: MerchantDetails,
    ) -> Result<MerchantProfile, String> {
        let merchant_id = self.merchant_id_counter.to_string();
        self.merchant_id_counter += 1;
        let merchant_profile = MerchantProfile {
            merchant_id: merchant_id.clone(),
            merchant_details: merchant_details,
        };
        self.merchants.insert(
          merchant_id.clone(),
          merchant_profile.clone());
        Ok(merchant_profile)
    }

    fn get_merchant(
        &self,
        merchant_id: MerchantId,
    ) -> Result<Option<MerchantProfile>, String> {
        match self.merchants.get(&merchant_id) {
            Some(merchant_profile) => Ok(Some(merchant_profile.clone())),
            None => Ok(None),
        }
    }
}
