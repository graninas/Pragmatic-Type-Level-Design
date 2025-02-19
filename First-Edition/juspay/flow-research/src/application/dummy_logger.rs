
use crate::application::services::ILogger;

// Dummy logger

pub struct DummyLogger;

impl DummyLogger {
  pub fn new() -> Self {
    Self {}
}
}

impl ILogger for DummyLogger {
  fn log(&self, message: String) {
    println!("{}", message);
  }
}
