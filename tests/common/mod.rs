pub type TestResult = Result<(), papyri_lang::utils::Diagnostics>;

#[macro_export]
macro_rules! assert_ok {
    ($($name: ident ($src: expr, $expected: expr$(,)?);)*) => {
        $(
            #[test]
            fn $name() -> $crate::common::TestResult {
                assert_eq!($expected, papyri_lang::compile_str($src)?);
                Ok(())
            }
        )*
    }
}
