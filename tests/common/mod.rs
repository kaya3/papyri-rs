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
    ($($name: ident ($src: expr, $category: ident :: $expected: ident $(,)?);)*) => {
        $(
            #[test]
            fn $name() -> $crate::common::TestResult {
                let Err(diagnostics) = papyri_lang::compile_str($src) else {
                    panic!("No errors");
                };
                if diagnostics.has_any(|d| matches!(d, papyri_lang::errors::PapyriError::$category(papyri_lang::errors::$category::$expected {..}))) {
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
                let val = $val;
                let pattern = $pattern;
                let src = format!("@match {val} {{ {pattern} -> OK, $val -> {{Failed: was $val}} }}");
                assert_eq!("<p>OK</p>", papyri_lang::compile_str(&src)?);
                Ok(())
            }
        )*
    }
}
