
#[cfg(test)]
mod tests {
  use super::*;

  use tl_list_lib::tl_list;
  use tl_list_lib::HList;
  use tl_list_lib::TlN_;
  use tl_list_lib::TlC_;
  use tl_interface::IInterface;
  use tl_interface::Wrapper;
  use tl_interface::EvalCtx;
  use tl_interface::Eval;
  use tl_str_list::TlStr;
  use tl_str_macro::tl_str;

  use std::marker::PhantomData;
  use std::any::Any;
  use std::collections::HashMap;

  use flow_research::tl_payment_processor_edsl::*;

  #[test]
  fn test_demo_payment_processors() {

    println!("{}", DemoPaymentProcessor::eval());
    println!("{}", PayPalProcessor::eval());

    // Uncomment to see the output
    // assert_eq!(true, false);

    // Test outputs:
    //
    // PaymentProcessor: DemoPaymentProcessor
    //   Features:
    //     - PaymentFeature: [UK] Card [USD] [3D Secure]
    //     - PaymentFeature: [UK] Card [EUR] [3D Secure]
    //     - PaymentFeature: [US] BNPL [USD] [Biometric]
    //   SupportedCountries:
    //     - US
    //   SupportedCurrencies:
    //     - USD
    //     - EUR
    //     - GBP
    //   SupportedMethods:
    //     - BNPL
    //     - Card
    //   Payment methods:
    //   - BNPL: [Funded by: Card] [DataSchema # of fields: 4]
    //   - Card: [DataSchema # of fields: 5]
    // PaymentProcessor: PayPalProcessor
    //   Features:
    //     - PaymentFeature: [US] PayPal [USD] [OTP]
    //     - PaymentFeature: [UK] PayPal [GBP] [3D Secure]
    //     - PaymentFeature: [FR] PayPal [EUR] [Biometric]
    //   SupportedCountries:
    //     - US
    //     - UK
    //     - FR
    //   SupportedCurrencies:
    //     - USD
    //     - EUR
    //     - GBP
    //   SupportedMethods:
    //     - PayPal
    //   Payment methods:
    //   - PayPal: [DataSchema # of fields: 3]
  }

}








