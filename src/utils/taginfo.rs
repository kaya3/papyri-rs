//! This module contains helper functions for some queries about HTML tags.

use super::NameID;
use super::str_ids;

#[derive(Debug, Clone, Copy)]
/// Specifies what contents/children an HTML tag can have.
pub enum ContentKind {
    /// Children must all be one of these specific tag kinds; wrap with the
    /// first one otherwise.
    RequireOneOf(&'static [NameID]),
    
    /// Children must all be blocks; wrap with this otherwise.
    RequireBlock(NameID),
    
    /// Children may be all inline or all blocks; wrap with this if necessary.
    AllowBlock(NameID),
    
    /// Children must be inline; convert paragraph breaks to `<br>` tags.
    RequireInline,
    
    /// Children must be inline; line breaks are forbidden.
    RequireInlineNoLineBreaks,
    
    /// No contents are allowed.
    RequireEmpty,
}

impl ContentKind {
    /// Content must be block-level; any inline content will be wrapped in
    /// `<p>` tags.
    pub const REQUIRE_P: ContentKind = ContentKind::RequireBlock(str_ids::P);
    
    /// Content may be either all block-level or all inline. If the content is
    /// mixed, then inline content will be wrapped in `<p>` tags.
    pub const ALLOW_P: ContentKind = ContentKind::AllowBlock(str_ids::P);
}

/// Indicates whether `name_id` is the id of a self-closing HTML tag name.
pub fn is_self_closing(name_id: NameID) -> bool {
    // https://developer.mozilla.org/en-US/docs/Glossary/Void_element
    matches!(
        name_id,
        str_ids::_DOCTYPE |
        str_ids::AREA |
        str_ids::BASE |
        str_ids::BR |
        str_ids::COL |
        str_ids::COMMAND |
        str_ids::EMBED |
        str_ids::HR |
        str_ids::IMG |
        str_ids::INPUT |
        str_ids::KEYGEN |
        str_ids::LINK |
        str_ids::MENUITEM |
        str_ids::META |
        str_ids::PARAM |
        str_ids::SOURCE |
        str_ids::TRACK |
        str_ids::WBR
    )
}

/// Indicates whether `name_id` is the id of a HTML block-level tag name, or
/// otherwise should not be wrapped in a block-level element.
pub fn is_block(name_id: NameID) -> bool {
    // https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
    // Extras:
    // - !DOCTYPE, body, canvas, head, html, menu, script, video
    matches!(
        name_id,
        str_ids::_DOCTYPE |
        str_ids::ADDRESS |
        str_ids::ARTICLE |
        str_ids::ASIDE |
        str_ids::BLOCKQUOTE |
        str_ids::BODY |
        str_ids::CANVAS |
        str_ids::DD |
        str_ids::DETAILS |
        str_ids::DIV |
        str_ids::DL |
        str_ids::DT |
        str_ids::FIELDSET |
        str_ids::FIGCAPTION |
        str_ids::FIGURE |
        str_ids::FOOTER |
        str_ids::FORM |
        str_ids::H1 |
        str_ids::H2 |
        str_ids::H3 |
        str_ids::H4 |
        str_ids::H5 |
        str_ids::H6 |
        str_ids::HEAD |
        str_ids::HEADER |
        str_ids::HGROUP |
        str_ids::HR |
        str_ids::HTML |
        str_ids::IMG |
        str_ids::LI |
        str_ids::MAIN |
        str_ids::MENU |
        str_ids::NAV |
        str_ids::OL |
        str_ids::P |
        str_ids::PRE |
        str_ids::SCRIPT |
        str_ids::SECTION |
        str_ids::TABLE |
        str_ids::UL |
        str_ids::VIDEO
    )
}

/// Returns the `ContentKind` specifying what contents/children are allowed for
/// an HTML tag of this name.
pub fn content_kind(name_id: NameID) -> ContentKind {
    match name_id {
        str_ids::ARTICLE |
        str_ids::ASIDE |
        str_ids::BLOCKQUOTE |
        str_ids::FOOTER |
        str_ids::HEADER |
        str_ids::MAIN |
        str_ids::NAV |
        str_ids::SECTION => ContentKind::REQUIRE_P,
        
        str_ids::ANONYMOUS |
        str_ids::ADDRESS |
        str_ids::BODY |
        str_ids::DETAILS |
        str_ids::DIV |
        str_ids::FIELDSET |
        str_ids::FIGCAPTION |
        str_ids::FIGURE |
        str_ids::FORM |
        str_ids::LI => ContentKind::ALLOW_P,
        
        str_ids::DL => ContentKind::RequireOneOf(&[
            str_ids::DD,
            str_ids::DT,
        ]),
        
        str_ids::HGROUP => ContentKind::RequireOneOf(&[
            str_ids::P,
            str_ids::H1,
            str_ids::H2,
            str_ids::H3,
            str_ids::H4,
            str_ids::H5,
            str_ids::H6,
        ]),
        
        str_ids::HTML => ContentKind::RequireOneOf(&[
            str_ids::BODY,
            str_ids::HEAD,
        ]),
        
        str_ids::MENU |
        str_ids::OL |
        str_ids::UL => ContentKind::RequireOneOf(&[
            str_ids::LI,
        ]),
        
        str_ids::TABLE => ContentKind::RequireOneOf(&[
            str_ids::TR,
            str_ids::TBODY,
            str_ids::TFOOT,
            str_ids::THEAD,
            str_ids::CAPTION,
            str_ids::COLGROUP,
        ]),
        
        str_ids::TBODY |
        str_ids::TFOOT |
        str_ids::THEAD => ContentKind::RequireOneOf(&[
            str_ids::TR,
        ]),
        
        str_ids::TR => ContentKind::RequireOneOf(&[
            str_ids::TD,
            str_ids::TH,
        ]),
        
        str_ids::H1 |
        str_ids::H2 |
        str_ids::H3 |
        str_ids::H4 |
        str_ids::H5 |
        str_ids::H6 |
        str_ids::P => ContentKind::RequireInlineNoLineBreaks,
        
        _ => if is_self_closing(name_id) {
            ContentKind::RequireEmpty
        } else {
            ContentKind::RequireInline
        },
    }
}
