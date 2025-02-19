use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

use crate::common_types::*;


#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Currency {            // AG: just a demo type
    USD,
    EUR,
    GBP,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Confirmation {
    Manual,
    Automatic,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum CaptureMethod {
    Manual,
    Automatic,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MerchantDetails {
    pub name: String,
    pub email: String,
    pub phone: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MerchantProfile {
    pub merchant_id: String,
    pub merchant_details: MerchantDetails,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CustomerDetails {
    pub name: Option<String>,
    pub email: Option<String>,
    pub phone: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CustomerProfile {
    pub customer_id: String,
    pub customer_details: CustomerDetails,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct OrderMetaData {
    pub order_id: String,
    pub order_description: String,
    pub encoded_details: Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PaymentData {
    pub amount: Amount,
    pub currency: Currency,
    pub payment_method: RawPaymentMethod,
    pub description: Option<String>,
    pub confirmation: Option<Confirmation>,
    pub capture_method: Option<CaptureMethod>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ThirdPartyPayment {
    pub third_party_payment_id: Option<ThirdPartyPaymentId>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Payment {
    pub payment_id: PaymentId,
    pub third_party_payment: ThirdPartyPayment,
    pub payment_data: PaymentData,
}
