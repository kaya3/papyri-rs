use papyri_lang::errors::PapyriError;

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
        "@let(x=5). $x $x",
        "<p>5 5</p>",
    );
}

assert_err! {
    raise(
        "@raise `foobar`",
        PapyriError::RuntimeError(..),
    );
}
