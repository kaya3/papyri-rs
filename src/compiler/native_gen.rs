#[macro_export]
/// This macro generates the declarations `enum NativeFunc`, `struct NativeDefs`
/// and `Compiler::evaluate_native_func`.
macro_rules! native_defs {
    (
        let $compiler: ident, $call_range: ident;
        $($func_name: ident ($($param_name: ident: $param_kind: ident $param_type: ty $(= $param_default: expr)?),*) $func_body: block)*
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum NativeFunc {
            $($func_name),*
        }
        
        impl NativeFunc {
            pub(super) fn name_id(self) -> NameID {
                match self {
                    $(NativeFunc::$func_name => str_ids::$func_name),*
                }
            }
        }
        
        #[allow(non_snake_case)]
        pub(super) struct NativeDefs {
            $(pub(super) $func_name: Func),*
        }
        
        impl NativeDefs {
            pub(super) fn build() -> NativeDefs {
                NativeDefs {
                    $($func_name: Func::Native(
                        NativeFunc::$func_name,
                        crate::compiler::signature::FuncSignature::builder()$(
                            .$param_kind(
                                crate::compiler::signature::FuncParam::new(str_ids::$param_name, <$param_type>::as_type())
                                $(.with_default(Value::from($param_default)))?
                            )
                        )*
                        .build()
                    )),*
                }
            }
        }
        
        impl <'a> Compiler<'a> {
            #[allow(non_snake_case, unreachable_code)]
            pub(super) fn evaluate_native_func(&mut self, f: NativeFunc, mut bindings: ValueMap, call_range: SourceRange) -> Option<Value> {
                let $compiler = self;
                let $call_range = call_range;
                match f {
                    $(NativeFunc::$func_name => {
                        $(let $param_name: $param_type = bindings.get_mut(&str_ids::$param_name)
                            .map(std::mem::take)
                            .unwrap_or_else(|| crate::errors::ice("failed to unpack"))
                            .expect_convert();)*
                        Some(Value::from($func_body))
                    })*
                }
            }
        }
    }
}
