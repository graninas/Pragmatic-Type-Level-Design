// Application-level infrastructure

pub trait ILogger {
    fn log(&self, message: String);
}

