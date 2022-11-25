pub type TestResult = Result<(), papyri_lang::errors::Diagnostics>;

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

#[macro_export]
macro_rules! assert_err {
    ($($name: ident ($src: expr, $expected: pat$(,)?);)*) => {
        $(
            #[test]
            fn $name() -> $crate::common::TestResult {
                let Err(diagnostics) = papyri_lang::compile_str($src) else {
                    panic!("No errors");
                };
                if diagnostics.has_any(|d| matches!(d, $expected)) {
                    Ok(())
                } else {
                    Err(diagnostics)
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! assert_matches {
    ($($name: ident ($val: expr, $pattern: expr$(,)?);)*) => {
        $(
            #[test]
            fn $name() -> $crate::common::TestResult {
                let src = format!("@match {} {{{} -> OK, _ -> Failed}}", $val, $pattern);
                assert_eq!("<p>OK</p>", papyri_lang::compile_str(&src)?);
                Ok(())
            }
        )*
    }
}
