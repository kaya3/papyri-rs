mod common;

assert_matches! {
    wildcard("23", "_");
    
    literal_int("23", "23");
    literal_str("`foobar`", "`foobar`");
    literal_bool("True", "True");
    literal_list("[1, 2]", "[1, 2]");
    literal_dict("@dict(x=1, y=2).", "(x=1, y=2)");
    
    typed_int("23", "_: int");
    typed_str("`foobar`", "_: str");
    typed_bool("True", "_: bool");
    typed_list("[1, 2]", "_: int list");
    typed_dict("@dict(x=1, y=2).", "_: int dict");
    typed_function("@fn $x -> $x", "_: function");
    
    spread_list("[1, 2, 3]", "[1, *[2, 3]]");
    spread_dict("@dict(x=1, y=2, z=3).", "(x=1, **(y=2, z=3))");
    
    tag_simple("<span>Foo</span>", "<span> _ </span>");
    tag_attr("<span id=`foobar`>Foo</span>", "<span id=`foobar`> _ </span>");
    tag_wildcard_name("<span>Foo</span>", "<_> _ </>");
}

assert_ok! {
    list_part(
        "@match [1, 2, 3] {[$x, *_] -> $x}",
        "<p>1</p>",
    );
    
    dict_part(
        "@match @dict(x=1, y=2, z=3). {(x=$x, **_) -> $x}",
        "<p>1</p>",
    );
    
    template_part(
        r#"@match `hello world` {"hello $x" -> $x}"#,
        "<p>world</p>",
    );
    
    type_of(
        "@match `foobar` {_: $t -> $t}",
        "<p>str</p>",
    );
}
