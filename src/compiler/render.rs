use std::io;
use htmlentity::entity;

use crate::utils::{StringPool, taginfo, str_ids, NameID};
use super::html::HTML;
use super::tag::{Tag, PlainTag};

impl HTML {
    fn escape(s: &str, escape_quotes: bool) -> String {
        entity::encode(
            s,
            if escape_quotes { entity::EntitySet::SpecialChars } else { entity::EntitySet::Html },
            entity::EncodeType::NamedOrHex,
        ).into_iter().collect()
    }
    
    pub fn unescape(s: &str) -> String {
        entity::decode(s).iter().collect::<String>()
    }
    
    pub fn render_to<T: io::Write>(&self, w: &mut T, string_pool: &mut StringPool, as_html: bool) -> Result<(), io::Error> {
        match self {
            HTML::DocType => {
                if as_html { write!(w, "<!DOCTYPE html>\n")?; }
            },
            HTML::Tag(tag) => {
                tag.render_to(w, string_pool, as_html)?;
            },
            HTML::PlainTag(tag) => {
                tag.render_to(w, string_pool, as_html)?;
            }
            HTML::Sequence(seq) => {
                for child in seq.iter() {
                    child.render_to(w, string_pool, as_html)?;
                }
            },
            HTML::Text(t) => {
                if as_html {
                    write!(w, "{}", HTML::escape(&t, false))?;
                } else {
                    write!(w, "{}", t)?;
                }
            },
            HTML::Whitespace => {
                write!(w, " ")?;
            },
            HTML::RawNewline => {
                write!(w, "\n")?;
            },
            HTML::Empty => {},
        }
        Ok(())
    }
    
    fn render_close_tag<T: io::Write>(w: &mut T, name_id: NameID, string_pool: &mut StringPool, as_html: bool) -> Result<(), io::Error> {
        if !as_html {
            match name_id {
                str_ids::P => write!(w, "\n\n")?,
                str_ids::HR => write!(w, "\n\u{2015}\n\n")?,
                name_id if taginfo::is_block(name_id) => write!(w, "\n")?,
                _ => {},
            }
        } else if !taginfo::is_self_closing(name_id) {
            write!(w, "</{}>", string_pool.get(name_id))?;
        }
        Ok(())
    }
}

impl PlainTag {
    fn render_to<T: io::Write>(&self, w: &mut T, string_pool: &mut StringPool, as_html: bool) -> Result<(), io::Error> {
        if as_html {
            write!(w, "<{}>", string_pool.get(self.name_id))?;
        }
        self.content.render_to(w, string_pool, as_html)?;
        HTML::render_close_tag(w, self.name_id, string_pool, as_html)?;
        Ok(())
    }
}

impl Tag {
    fn render_to<T: io::Write>(&self, w: &mut T, string_pool: &mut StringPool, as_html: bool) -> Result<(), io::Error> {
        if as_html {
            write!(w, "<{}", string_pool.get(self.name_id))?;
            for (k, v) in &self.attributes {
                write!(w, " {}", string_pool.get(*k))?;
                if let Some(v) = v {
                    write!(w, "=\"{}\"", HTML::escape(v, true))?;
                }
            }
            if !self.css_classes.is_empty() {
                write!(w, " class=\"")?;
                let mut space = false;
                for v in &self.css_classes {
                    if space { write!(w, " ")?; }
                    write!(w, "{}", v)?;
                    space = true;
                }
                write!(w, "\"")?;
            }
            if let Some(style) = &self.css_style {
                write!(w, " style=\"{}\"", HTML::escape(style, true))?;
            }
            write!(w, ">")?;
        }
        
        self.content.render_to(w, string_pool, as_html)?;
        HTML::render_close_tag(w, self.name_id, string_pool, as_html)?;
        Ok(())
    }
}
