#[macro_export]
/// This macro generates the declarations `enum NativeFunc`, `struct NativeDefs`
/// and `Compiler::evaluate_native_func`.
macro_rules! native_defs {
    (
        let $compiler: ident, $call_range: ident;
        $(impl $type_name: ident {
            $(fn $m_name: ident ($($m_param_name: ident: $m_param_kind: ident $m_param_type: ty $(= $m_param_default: expr)?),*) $m_body: block)*
        })*
        $(fn $f_name: ident ($($f_param_name: ident: $f_param_kind: ident $f_param_type: ty $(= $f_param_default: expr)?),*) $f_body: block)*
    ) => {
        #[allow(non_camel_case_types)]
        mod native_names {
            $(
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                pub enum $type_name {
                    $($m_name),*
                }
            )*
            
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum NativeFunc {
                $($type_name($type_name),)*
                $($f_name),*
            }
        }
        pub use native_names::NativeFunc;
        
        impl NativeFunc {
            pub(super) fn name_id(self) -> crate::utils::NameID {
                match self {
                    $($(NativeFunc::$type_name(native_names::$type_name::$m_name) => crate::utils::str_ids::$m_name,)*)*
                    $(NativeFunc::$f_name => crate::utils::str_ids::$f_name),*
                }
            }
        }
        
        #[allow(non_snake_case)]
        mod native_cache {
            $(
                pub(crate) struct $type_name {
                    $(pub(crate) $m_name: crate::compiler::func::Func),*
                }
            )*
            
            pub(crate) struct NativeDefs {
                $(pub(crate) $type_name: $type_name,)*
                $(pub(crate) $f_name: crate::compiler::func::Func),*
            }
        }
        pub(super) use native_cache::NativeDefs;
        
        impl NativeDefs {
            pub(super) fn build() -> NativeDefs {
                NativeDefs {
                    $($type_name: native_cache::$type_name {
                        $($m_name: Func::Native(
                            NativeFunc::$type_name(native_names::$type_name::$m_name),
                            crate::compiler::signature::FuncSignature::builder()
                            $(.$m_param_kind(
                                crate::compiler::signature::FuncParam::new(crate::utils::str_ids::$m_param_name, <$m_param_type>::as_type())
                                $(.with_default(crate::compiler::value::Value::from($m_param_default)))?
                            ))*
                            .build()
                        )),*
                    },)*
                    $($f_name: Func::Native(
                        NativeFunc::$f_name,
                        crate::compiler::signature::FuncSignature::builder()
                        $(.$f_param_kind(
                            crate::compiler::signature::FuncParam::new(crate::utils::str_ids::$f_param_name, <$f_param_type>::as_type())
                            $(.with_default(crate::compiler::value::Value::from($f_param_default)))?
                        ))*
                        .build()
                    )),*
                }
            }
            
            #[allow(non_snake_case, unused_must_use)]
            pub(super) fn to_frame(&self) -> crate::compiler::frame::ActiveFrame {
                let globals = crate::compiler::value::ValueMap::from_iter([
                    $((
                        crate::utils::str_ids::$type_name,
                        crate::compiler::value::ValueMap::from_iter([
                            $((
                                crate::utils::str_ids::$m_name,
                                self.$type_name.$m_name.clone().into()
                            ),)*
                        ]).into(),
                    ),)*
                    $((crate::utils::str_ids::$f_name, self.$f_name.clone().into())),*
                ]);
                
                crate::compiler::frame::ActiveFrame::new(None, globals, None)
            }
        }

        impl <'a> crate::compiler::base::Compiler<'a> {
            #[allow(non_snake_case, unreachable_code)]
            pub(super) fn evaluate_native_func(&mut self, f: NativeFunc, mut bindings: crate::compiler::value::ValueMap, $call_range: crate::utils::sourcefile::SourceRange) -> Option<crate::compiler::value::Value> {
                let $compiler = self;
                match f {
                    $($(NativeFunc::$type_name(native_names::$type_name::$m_name) => {
                        $(let $m_param_name: $m_param_type = bindings.get_mut(&crate::utils::str_ids::$m_param_name)
                            .map(std::mem::take)
                            .unwrap_or_else(|| crate::errors::ice("failed to unpack"))
                            .expect_convert();)*
                        Some(crate::compiler::value::Value::from($m_body))
                    })*)*
                    $(NativeFunc::$f_name => {
                        $(let $f_param_name: $f_param_type = bindings.get_mut(&crate::utils::str_ids::$f_param_name)
                            .map(std::mem::take)
                            .unwrap_or_else(|| crate::errors::ice("failed to unpack"))
                            .expect_convert();)*
                        Some(crate::compiler::value::Value::from($f_body))
                    })*
                }
            }
        }
    }
}
