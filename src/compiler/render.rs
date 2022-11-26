use std::io;

use crate::utils::{StringPool, taginfo, str_ids};
use crate::parser::text;
use super::html::HTML;
use super::tag::Tag;

impl HTML {
    pub fn render_to<T: io::Write>(&self, w: &mut T, string_pool: &StringPool, as_html: bool) -> Result<(), io::Error> {
        match self {
            HTML::Tag(tag) => {
                tag.render_to(w, string_pool, as_html)?;
            },
            HTML::Sequence(seq) => {
                for child in seq.iter() {
                    child.render_to(w, string_pool, as_html)?;
                }
            },
            HTML::Text(t) => {
                if as_html {
                    write!(w, "{}", text::encode_entities(&t, false))?;
                } else {
                    write!(w, "{t}")?;
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
}

impl Tag {
    fn render_to<T: io::Write>(&self, w: &mut T, string_pool: &StringPool, as_html: bool) -> Result<(), io::Error> {
        if as_html {
            let name = string_pool.get(self.name_id);
            write!(w, "<{name}")?;
            for (k, v) in &self.attributes {
                write!(w, " {}", string_pool.get(*k))?;
                if let Some(v) = v {
                    write!(w, "=\"{}\"", text::encode_entities(v, true))?;
                }
            }
            write!(w, ">")?;
        }
        
        self.content.render_to(w, string_pool, as_html)?;
        if !as_html {
            match self.name_id {
                str_ids::P => write!(w, "\n\n")?,
                str_ids::HR => write!(w, "\n\u{2015}\n\n")?,
                name_id if taginfo::is_block(name_id) => writeln!(w)?,
                _ => {},
            }
        } else if !taginfo::is_self_closing(self.name_id) {
            let name = string_pool.get(self.name_id);
            write!(w, "</{name}>")?;
        } else if self.name_id == str_ids::_DOCTYPE {
            writeln!(w)?;
        }
        Ok(())
    }
}
