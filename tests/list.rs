mod common;

assert_ok! {
    basic_ul(
        "[Hello, world]",
        "<ul><li>Hello</li><li>world</li></ul>",
    );
    
    basic_ol(
        "@numbered [Hello, world]",
        "<ol><li>Hello</li><li>world</li></ol>",
    );
    
    nested(
        "[Hello, [Papyri, world]]",
        "<ul><li>Hello</li><li><ul><li>Papyri</li><li>world</li></ul></li></ul>",
    );
    
    spread(
        "[Hello, *[Papyri, world]]",
        "<ul><li>Hello</li><li>Papyri</li><li>world</li></ul>",
    );
}
