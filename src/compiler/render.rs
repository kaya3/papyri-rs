use std::io;

use crate::utils::{StringPool, taginfo, str_ids};
use crate::parser::text;
use super::html::HTML;
use super::tag::Tag;

pub struct Renderer<'a, T: io::Write> {
    string_pool: &'a StringPool,
    as_html: bool,
    writer: &'a mut T,
}

impl <'a, T: io::Write> Renderer<'a, T> {
    pub fn new(string_pool: &'a StringPool, as_html: bool, writer: &'a mut T) -> Renderer<'a, T> {
        Renderer {string_pool, as_html, writer}
    }
    
    pub fn render(&mut self, html: &HTML) -> Result<(), io::Error> {
        match html {
            HTML::Tag(tag) => {
                self.render_tag(tag)?;
            },
            HTML::Sequence(seq) => {
                for child in seq.iter() {
                    self.render(child)?;
                }
            },
            HTML::Text(t) => {
                if self.as_html {
                    write!(self.writer, "{}", text::encode_entities(&t, false))?;
                } else {
                    write!(self.writer, "{t}")?;
                }
            },
            HTML::Whitespace => {
                write!(self.writer, " ")?;
            },
            HTML::RawNewline => {
                writeln!(self.writer)?;
            },
            HTML::Empty => {},
        }
        Ok(())
    }
    
    fn render_tag(&mut self, tag: &Tag) -> Result<(), io::Error> {
        let name = self.string_pool.get(tag.name_id);
        if self.as_html {
            write!(self.writer, "<{name}")?;
            for (&k, v) in tag.attributes.iter() {
                write!(self.writer, " {}", self.string_pool.get(k))?;
                if let Some(v) = v {
                    write!(self.writer, "=\"{}\"", text::encode_entities(v, true))?;
                }
            }
            write!(self.writer, ">")?;
        }
        
        self.render(&tag.content)?;
        if !self.as_html {
            match tag.name_id {
                str_ids::P => write!(self.writer, "\n\n")?,
                str_ids::HR => write!(self.writer, "\n\u{2015}\n\n")?,
                name_id if taginfo::is_block(name_id) => writeln!(self.writer)?,
                _ => {},
            }
        } else if !taginfo::is_self_closing(tag.name_id) {
            write!(self.writer, "</{name}>")?;
        } else if tag.name_id == str_ids::_DOCTYPE {
            writeln!(self.writer)?;
        }
        Ok(())
    }
}
