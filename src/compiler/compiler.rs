use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{parse, AST, text};
use crate::utils::taginfo::ContentKind;
use crate::utils::{Diagnostics, ice_at, ice, SourceFile, SourceRange, str_ids, NameID};

use super::frame::{ActiveFrame, InactiveFrame};
use super::html::HTML;
use super::loader::ModuleLoader;
use super::native::get_natives_frame;
use super::types::Type;
use super::value::{Value, ValueMap};

pub struct CompileResult {
    pub out: HTML,
    pub exports: Rc<ValueMap>,
}

pub fn compile(src: Rc<SourceFile>, loader: &mut ModuleLoader, diagnostics: &mut Diagnostics) -> CompileResult {
    let root = parse(src, diagnostics, &mut loader.string_pool);
    let mut compiler = Compiler::new(diagnostics, loader);
    let out = compiler.compile_sequence(&root, ContentKind::REQUIRE_P);
    CompileResult {
        out,
        exports: Rc::new(compiler.exports),
    }
}

pub fn compile_stdlib(loader: &mut ModuleLoader) -> InactiveFrame {
    let stdlib_src = SourceFile::synthetic("<stdlib>", include_str!("../std.papyri"));
    
    let mut diagnostics = Diagnostics::new();
    let root = parse(stdlib_src, &mut diagnostics, &mut loader.string_pool);
    let mut compiler = Compiler::new(&mut diagnostics, loader);
    let result = compiler.compile_sequence(&root, ContentKind::REQUIRE_P);
    
    if !compiler.diagnostics.is_empty() {
        diagnostics.print(false);
        ice("Standard library had errors or warnings");
    } else if !matches!(result, HTML::Empty) {
        ice("Standard library had non-empty output");
    }
    InactiveFrame::from(compiler.frame)
}

pub struct Compiler<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub loader: &'a mut ModuleLoader,
    pub frame: ActiveFrame,
    pub exports: ValueMap,
}

impl <'a> Compiler<'a> {
    fn new(diagnostics: &'a mut Diagnostics, loader: &'a mut ModuleLoader) -> Compiler<'a> {
        let frame = match loader.stdlib.as_ref() {
            Some(stdlib) => stdlib.new_child_frame(HashMap::new()),
            None => get_natives_frame(),
        };
        Compiler {
            diagnostics,
            loader,
            frame,
            exports: HashMap::new(),
        }
    }
    
    pub fn get_name(&self, name_id: NameID) -> &str {
        self.loader.string_pool.get(name_id)
    }
    
    pub fn compile_node(&mut self, node: &AST) -> HTML {
        match node {
            AST::FuncDef(def) => {
                if def.name_id.is_anonymous() {
                    self.diagnostics.warning("anonymous function not expected here", &def.range);
                } else {
                    let f = self.compile_func_def(def);
                    self.set_var(def.name_id, Value::Func(f), false, node.range());
                }
                HTML::Empty
            },
            
            AST::Group(group, ..) => self.compile_sequence(group, ContentKind::ALLOW_P),
            AST::Tag(tag) => self.compile_tag(tag),
            
            AST::FuncCall(..) |
            AST::Match(..) |
            AST::VarName(..) |
            AST::Verbatim(..) |
            AST::List(..) => {
                self.evaluate_node(node, &Type::AnyHTML)
                    .map_or(HTML::Empty, |v| self.compile_value(v))
            },
            
            AST::LiteralValue(tok) => tok.text().map(text::substitutions).map_or(HTML::Empty, HTML::from),
            AST::Text(text, ..) => HTML::Text(text.clone()),
            AST::Whitespace(..) => HTML::Whitespace,
            AST::Escape(range) => self.compile_escape(range),
            AST::Entity(range) => HTML::Text(Rc::from(text::decode_entity(range, self.diagnostics))),
            
            AST::ParagraphBreak(range) => ice_at("paragraph break should be handled in SequenceCompiler", range),
            AST::Template(.., range) => ice_at("template should not occur in non-value context", range),
        }
    }
    
    fn compile_escape(&mut self, range: &SourceRange) -> HTML {
        match range.as_str() {
            "\\n" => HTML::tag(str_ids::BR, HTML::Empty),
            _ => HTML::text(self.unescape_char(range).encode_utf8(&mut [0; 4])),
        }
    }
    
    pub fn unescape_char(&mut self, range: &SourceRange) -> char {
        text::unescape_char(range, self.diagnostics)
    }
}
