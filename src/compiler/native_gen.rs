#[macro_export]
/// This macro generates the declarations `enum NativeFunc`, `struct NativeDefs`,
/// `Compiler::evaluate_native_func` and `Compiler::evaluate_native_attr`.
macro_rules! native_defs {
    (
        let $compiler: ident, $call_range: ident;
        $(impl $type_name: ident $(for $type_variant: ident)? {
            $($(@$m_bind: ident)? fn $m_name: ident ($($m_param_name: ident: $m_param_kind: ident $m_param_type: ty $(= $m_param_default: expr)?),*) $m_body: block)*
        })*
        $(fn $f_name: ident ($($f_param_name: ident: $f_param_kind: ident $f_param_type: ty $(= $f_param_default: expr)?),*) $f_body: block)*
    ) => {
        #[allow(non_camel_case_types, clippy::upper_case_acronyms, clippy::enum_variant_names)]
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
        
        /*
        impl NativeFunc {
            pub(super) fn name_id(self) -> $crate::utils::NameID {
                use $crate::utils::str_ids;
                match self {
                    $($(NativeFunc::$type_name(native_names::$type_name::$m_name) => str_ids::$m_name,)*)*
                    $(NativeFunc::$f_name => str_ids::$f_name),*
                }
            }
        }*/
        
        #[allow(non_snake_case, clippy::upper_case_acronyms)]
        mod native_cache {
            use $crate::compiler::func::Func;
            $(
                pub(crate) struct $type_name {
                    $(pub(crate) $m_name: Func),*
                }
            )*
            
            pub(crate) struct NativeDefs {
                $(pub(crate) $type_name: $type_name,)*
                $(pub(crate) $f_name: Func),*
            }
        }
        pub(super) use native_cache::NativeDefs;
        
        impl NativeDefs {
            pub(super) fn build() -> NativeDefs {
                use $crate::utils::str_ids;
                use $crate::compiler::signature::{FuncParam, FuncSignature};
                use $crate::compiler::value::Value;
                NativeDefs {
                    $($type_name: native_cache::$type_name {
                        $($m_name: Func::Native(
                            NativeFunc::$type_name(native_names::$type_name::$m_name),
                            str_ids::$m_name,
                            FuncSignature::builder()
                            $(.$m_param_kind(
                                FuncParam::new(str_ids::$m_param_name, <$m_param_type>::as_type())
                                $(.with_default(Value::from($m_param_default)))?
                            ))*
                            .build()
                        )),*
                    },)*
                    $($f_name: Func::Native(
                        NativeFunc::$f_name,
                        str_ids::$f_name,
                        FuncSignature::builder()
                        $(.$f_param_kind(
                            FuncParam::new(str_ids::$f_param_name, <$f_param_type>::as_type())
                            $(.with_default(Value::from($f_param_default)))?
                        ))*
                        .build()
                    )),*
                }
            }
            
            #[allow(non_snake_case, unused_must_use)]
            pub(super) fn to_frame(&self) -> $crate::compiler::frame::ActiveFrame {
                use $crate::utils::str_ids;
                use $crate::compiler::value::ValueMap;
                use $crate::compiler::frame::ActiveFrame;
                let globals = ValueMap::from_iter([
                    $((
                        str_ids::$type_name,
                        ValueMap::from_iter([
                            $((
                                str_ids::$m_name,
                                self.$type_name.$m_name.clone().into()
                            ),)*
                        ]).into(),
                    ),)*
                    $((str_ids::$f_name, self.$f_name.clone().into())),*
                ]);
                ActiveFrame::new(None, globals, None)
            }
        }
        
        impl <'a> $crate::compiler::base::Compiler<'a> {
            #[allow(non_snake_case, unreachable_code)]
            pub(super) fn evaluate_native_func(&mut self, f: NativeFunc, mut bindings: $crate::compiler::value::ValueMap, $call_range: $crate::utils::sourcefile::SourceRange) -> Option<$crate::compiler::value::Value> {
                use $crate::utils::{str_ids, NameID};
                use $crate::errors;
                use $crate::compiler::value::Value;
                let $compiler = self;
                let mut take = |name_id: NameID| {
                    bindings.get_mut(&name_id)
                        .map(std::mem::take)
                        .unwrap_or_else(|| errors::ice("failed to unpack"))
                };
                Some(match f {
                    $($(NativeFunc::$type_name(native_names::$type_name::$m_name) => {
                        $(let $m_param_name: $m_param_type = take(str_ids::$m_param_name).expect_convert();)*
                        Value::from($m_body)
                    })*)*
                    $(NativeFunc::$f_name => {
                        $(let $f_param_name: $f_param_type = take(str_ids::$f_param_name).expect_convert();)*
                        Value::from($f_body)
                    })*
                })
            }
            
            pub(super) fn evaluate_native_attr(&mut self, subject: $crate::compiler::value::Value, attr_id: $crate::utils::NameID, attr_range: $crate::utils::sourcefile::SourceRange) -> Option<$crate::compiler::func::Func> {
                use $crate::utils::str_ids;
                use $crate::compiler::value::Value;
                $(if false $(|| matches!(subject, Value::$type_variant(..)))? {
                    let _cls = &self.ctx.natives.$type_name;
                    match attr_id {
                        $($(str_ids::$m_name => return _cls.$m_name.clone().$m_bind(self, subject, attr_range),)?)*
                        _ => {},
                    }
                }) else*
                None
            }
        }
    }
}
