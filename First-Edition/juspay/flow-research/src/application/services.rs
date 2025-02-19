use either::Either;
use serde_json::Value;
use serde::{Serialize, Deserialize};

// Infrastructure

pub trait ILogger {
    fn log(&self, message: String);
}

