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
    
    variable(
        "@let(x=5) {$x $x}",
        "<p>5 5</p>",
    );
    
    code_inline(
        "`some_code`",
        "<p><code>some_code</code></p>",
    );
}

assert_err! {
    raise("@raise `foobar`", RuntimeError::Raised);
}
