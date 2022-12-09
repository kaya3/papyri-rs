mod common;

assert_ok! {
    hello_world(
        "Hello, world!",
        "<p>Hello, world!</p>",
    );
    
    paragraph_break(
        "Paragraph 1\n\nParagraph 2",
        "<p>Paragraph 1</p><p>Paragraph 2</p>",
    );
    
    code_inline(
        "`some_code`",
        "<p><code>some_code</code></p>",
    );
    
    variable(
        "@let(x=5) {$x $x}",
        "<p>5 5</p>",
    );
    
    line_comment(
        "# Nothing\nSomething",
        "<p>Something</p>",
    );
    
    multiline_comment(
        "<!-- Nothing\nhere -->\nSomething",
        "<p>Something</p>",
    );
}

assert_matches! {
    let_multiple(
        "@let(x=5, y=$x) $y",
        "5",
    );
}
