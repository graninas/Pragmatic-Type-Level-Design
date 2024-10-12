// Macro for generating kind evidences and instances
#[macro_export]
macro_rules! generate_kind_evidence {
    ($type:ty, $trait:ident, $struct:ident, $const_name:ident) => {
        impl $trait for $type {}

        #[allow(dead_code)]
        const $const_name: $struct<$type> = $struct::<$type> {
            _marker: PhantomData::<$type>,
        };
    };
}
