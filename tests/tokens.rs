use papyri_lang::errors::PapyriError;

mod common;

assert_ok! {
    verbatim_with_backslash(
        r"@str `\`",
        r"<p>\</p>",
    );
    
    verbatim_with_line_comment(
        "@str `#`",
        "<p>#</p>",
    );
    
    verbatim_with_html_comment(
        "@str `<!--`",
        "<p>&lt;!--</p>",
    );
    
    line_comment(
        "# Nothing\nSomething",
        "<p>Something</p>",
    );
    
    multiline_comment(
        "<!-- Nothing\nhere -->\nSomething",
        "<p>Something</p>",
    );
    
    line_comment_with_backticks(
        "# ```\nFoobar",
        "<p>Foobar</p>",
    );
    
    multiline_comment_with_escape(
        r"<!-- \-->Foobar",
        "<p>Foobar</p>",
    );
    
    multiline_comment_nested(
        r"<!-- <!-->Foobar",
        "<p>Foobar</p>",
    );
    
    multiline_comment_with_backticks(
        r"<!-- ``` -->Foobar",
        "<p>Foobar</p>",
    );
}

assert_err! {
    verbatim_eof(
        "`some string literal",
        PapyriError::SyntaxError(..),
    );
}
