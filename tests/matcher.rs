mod common;

assert_matches! {
    wildcard("23", "_");
    
    literal_int("23", "23");
    literal_str("`foobar`", "`foobar`");
    literal_bool("True", "True");
    literal_list("[1, 2]", "[1, 2]");
    literal_dict("@dict(x=1, y=2).", "(x=1, y=2)");
    
    unit_dot_dot(".", ".");
    unit_dot_braces(".", "{}");
    unit_braces_dot("{}", ".");
    unit_braces_braces("{}", "{}");
    typed_unit_dot(".", "_: none");
    typed_unit_braces("{}", "_: none");
    
    typed_int("23", "_: int");
    typed_str("`foobar`", "_: str");
    typed_bool("True", "_: bool");
    typed_list("[1, 2]", "_: int list");
    typed_dict("@dict(x=1, y=2).", "_: int dict");
    typed_function("@fn $x -> $x", "_: function");
    
    spread_list("[1, 2, 3]", "[1, *[2, 3]]");
    spread_dict("@dict(x=1, y=2, z=3).", "(x=1, **(y=2, z=3))");
    equals_dict("@dict(x=1, y=2, z=3).", "=@dict(z=3, y=2, x=1).");
    
    tag_simple("<span>Foo</span>", "<span> _ </span>");
    tag_empty("<span/>", "<span/>");
    tag_attr("<span id=`foobar`>Foo</span>", "<span id=`foobar`> _ </span>");
    tag_wildcard_name("<span>Foo</span>", "<_> _ </>");
    tag_seq("{<a/><b/><i/>}", "{<a/><b/><i/>}");
    tag_spread("{<a/><b/><i/><u/>}", "{<a/> *_ <u/>}");
    tag_one_in_seq("<span/>", "{<span/>}");
    
    equals_captured_var("[23, 23]", "[$x, =$x]");
}

assert_ok! {
    equals_value(
        "@let(x=23, y=17) @match 17 {=$x -> {Failed (equals x)}, =$y -> OK, _ -> Failed}",
        "<p>OK</p>"
    );
    
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
    
    tag_part(
        "@match {<a/><b>Foo</b><i/>} {{<a/>$x<i/>} -> $x}",
        "<p><b>Foo</b></p>"
    );
    
    tag_spread_part(
        "@match {<a/><b>Foo</b><b>Bar</b><i/>} {{<a/>*$x<i/>} -> $x}",
        "<p><b>Foo</b><b>Bar</b></p>"
    );
    
    type_of(
        "@match `foobar` {_: $t -> $t}",
        "<p>str</p>",
    );
}

assert_err! {
    literal_in_html("@match 5 {{5} -> OK}", SyntaxError::PatternCannotMatchHTML);
}
