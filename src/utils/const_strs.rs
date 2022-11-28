use super::stringpool::NameID;

macro_rules! const_strs {
    (@count () {$id: expr}) => {};
    (@count ($name_head: ident $($name_tail: ident)*) {$id: expr}) => {
        pub const $name_head: NameID = NameID($id);
        const_strs!(@count ($($name_tail)*) {$id + 1});
    };
    
    ($($name: ident = $val: expr,)*) => {
        pub static CONST_STRS: &'static [&'static str] = &[
            $($val,)*
        ];
        
        /// Constants for ids of names which are always pooled.
        pub mod str_ids {
            use super::NameID;
            const_strs!{@count ($($name)*) {0}}
        }
    }
}

const_strs!(
    ANONYMOUS = "<anonymous>",
    _DOCTYPE = "!DOCTYPE",
    _0 = "_0",
    A = "a",
    ADDRESS = "address",
    AREA = "area",
    ARGS = "args",
    ARTICLE = "article",
    ASIDE = "aside",
    BASE = "base",
    BLOCKQUOTE = "blockquote",
    BODY = "body",
    BR = "br",
    CANVAS = "canvas",
    CAPTION = "caption",
    CLASS = "class",
    CODE = "code",
    CODE_BLOCK = "code_block",
    COL = "col",
    COLGROUP = "colgroup",
    COMMAND = "command",
    CONTENT = "content",
    DATA_LINE_NO = "data-line-no",
    DATA_PAREN_NO = "data-paren-no",
    DD = "dd",
    DETAILS = "details",
    DIV = "div",
    DL = "dl",
    DT = "dt",
    EMBED = "embed",
    EXPORT = "export",
    FIELDSET = "fieldset",
    FIGCAPTION = "figcaption",
    FIGURE = "figure",
    FIRST_LINE_NO = "first_line_no",
    FIX_INDENTATION = "fix_indentation",
    FOOTER = "footer",
    FORM = "form",
    H1 = "h1",
    H2 = "h2",
    H3 = "h3",
    H4 = "h4",
    H5 = "h5",
    H6 = "h6",
    HEAD = "head",
    HEADER = "header",
    HGROUP = "hgroup",
    HR = "hr",
    HREF = "href",
    HTML = "html",
    IMG = "img",
    IMPLICIT = "implicit",
    IMPORT = "import",
    INCLUDE = "include",
    INPUT = "input",
    KEYGEN = "keygen",
    LANGUAGE = "language",
    LET = "let",
    LI = "li",
    LINK = "link",
    MAIN = "main",
    MAP = "map",
    MENU = "menu",
    MENUITEM = "menuitem",
    META = "meta",
    NAV = "nav",
    OL = "ol",
    P = "p",
    PARAM = "param",
    PRE = "pre",
    RAISE = "raise",
    REL = "rel",
    SCRIPT = "script",
    SECTION = "section",
    SOURCE = "source",
    SPAN = "span",
    SRC = "src",
    STYLE = "style",
    SYNTAX_HIGHLIGHT = "syntax_highlight",
    TABLE = "table",
    TBODY = "tbody",
    TD = "td",
    TFOOT = "tfoot",
    TH = "th",
    THEAD = "thead",
    TITLE = "title",
    TR = "tr",
    TRACK = "track",
    TYPE = "type",
    UL = "ul",
    VIDEO = "video",
    WBR = "wbr",
    WRITE_FILE = "write_file",
);
