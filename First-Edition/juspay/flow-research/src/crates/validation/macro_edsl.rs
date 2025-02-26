

macro_rules! mandatory {
  ($validated:expr, $field:expr) => {
      Box::new(|| {
        match &$validated {
          Right(value) => {
            $field = value.clone();
            Ok(())
          },
          Left(e) => Err(format!("Validation failed: {:?}", e)),
        }
      })
  };
}

macro_rules! optional {
  ($validated:expr, $default:expr, $field:expr) => {
    Box::new(|| {
        match &$validated {
          Right(value) => {
            $field = value.clone();
            Ok(())
          },
          Left(ValidationResult::Missing(_)) => {
            $field = $default.clone();
            Ok(())
          },
          Left(e) => Err(format!("Validation failed: {:?}", e)),
        }
      })
  };
}
