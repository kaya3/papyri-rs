mod common;

assert_matches! {
    verbatim_with_backslash(
        r"`\`",
        r"='\\'",
    );
    
    verbatim_with_line_comment(
        r"`#`",
        r"='\#'",
    );
    
    verbatim_with_html_comment(
        r"`<!--`",
        r"='\<!--'",
    );
}

assert_ok! {
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
    verbatim_eof("`some string literal", SyntaxError::TokenVerbatimEOF);
}
