use std::rc::Rc;

use crate::errors::{Diagnostics, ice, ice_at, Warning, RuntimeError, ReportingLevel, RuntimeWarning};
use crate::parser::{parse, AST, text};
use crate::utils::{SourceFile, SourceRange, NameID, OutFiles, taginfo};
use super::frame::{ActiveFrame, InactiveFrame};
use super::html::HTML;
use super::loader::ModuleLoader;
use super::types::Type;
use super::value::{Value, ValueMap};

/// The result of compiling a Papyri source file. The output may be incomplete
/// if there were errors during compilation. 
pub struct CompileResult {
    /// The direct HTML output from compiling a Papyri source file. Other output
    /// may have been queued in an `OutFiles` collector, if the Papyri source
    /// file used the `@write_file` function to produce its output.
    pub out: HTML,
    
    /// The values exported by this Papyri source file, using the `@export`
    /// function.
    pub exports: ValueMap,
}

/// Compiles a Papyri source file.
pub fn compile(src: Rc<SourceFile>, loader: &mut ModuleLoader, diagnostics: &mut Diagnostics, out_files: Option<&mut OutFiles<HTML>>) -> CompileResult {
    let root = parse(src, diagnostics, &mut loader.string_pool);
    let mut compiler = Compiler::new(diagnostics, loader, out_files);
    let out = compiler.compile_sequence(&root, taginfo::ContentKind::REQUIRE_P);
    CompileResult {
        out,
        exports: compiler.exports,
    }
}

pub fn compile_stdlib(loader: &mut ModuleLoader) -> InactiveFrame {
    let stdlib_src = SourceFile::synthetic("stdlib", include_str!("../std.papyri"));
    
    let mut diagnostics = Diagnostics::new(ReportingLevel::All);
    let root = parse(stdlib_src, &mut diagnostics, &mut loader.string_pool);
    let mut compiler = Compiler::new(&mut diagnostics, loader, None);
    let result = compiler.compile_sequence(&root, taginfo::ContentKind::REQUIRE_P);
    
    if !compiler.diagnostics.is_empty() {
        diagnostics.print();
        ice("Standard library had errors or warnings");
    } else if !compiler.exports.is_empty() {
        ice("Standard library had exports");
    } else if !result.is_empty() {
        ice("Standard library had non-empty output");
    }
    InactiveFrame::from(compiler.call_stack.pop().unwrap())
}

pub struct Compiler<'a> {
    pub diagnostics: &'a mut Diagnostics,
    pub loader: &'a mut ModuleLoader,
    pub out_files: Option<&'a mut OutFiles<HTML>>,
    pub call_stack: Vec<ActiveFrame>,
    pub exports: ValueMap,
}

impl <'a> Compiler<'a> {
    fn new(diagnostics: &'a mut Diagnostics, loader: &'a mut ModuleLoader, out_files: Option<&'a mut OutFiles<HTML>>) -> Compiler<'a> {
        let frame = loader.get_initial_frame();
        Compiler {
            diagnostics,
            loader,
            out_files,
            call_stack: vec![frame],
            exports: ValueMap::new(),
        }
    }
    
    pub fn runtime_error(&mut self, e: RuntimeError, range: &SourceRange) {
        self.diagnostics.runtime_error(e, self.stack_trace(), range);
    }
    
    pub fn runtime_warning(&mut self, e: RuntimeWarning, range: &SourceRange) {
        self.diagnostics.runtime_warning(e, self.stack_trace(), range);
    }
    
    pub fn get_name(&self, name_id: NameID) -> &str {
        self.loader.string_pool.get(name_id)
    }
    
    pub fn compile_node(&mut self, node: &AST) -> HTML {
        match node {
            AST::FuncDef(def) => {
                if def.name_id.is_anonymous() {
                    self.diagnostics.warning(Warning::AnonymousFunctionInText, &def.range);
                } else {
                    let f = self.compile_func_def(def);
                    self.set_var(def.name_id, Value::Func(f), false, node.range());
                }
                HTML::Empty
            },
            
            AST::Group(group, ..) => self.compile_sequence(group, taginfo::ContentKind::ALLOW_P),
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
            
            AST::ParagraphBreak(range) => ice_at("paragraph break should be handled in SequenceCompiler", range),
            AST::Template(.., range) => ice_at("template should not occur in non-value context", range),
        }
    }
}
