mod common;

assert_matches! {
    wildcard("23", "_");
    
    literal_int("23", "23");
    literal_str("`foobar`", "`foobar`");
    literal_bool("True", "True");
    literal_list("[1, 2]", "[1, 2]");
    literal_dict("@dict::new(x=1, y=2).", "(x=1, y=2)");
    
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
    typed_dict("@dict::new(x=1, y=2).", "_: int dict");
    typed_function("@fn $x -> $x", "_: function");
    
    spread_list("[1, 2, 3]", "[1, *[2, 3]]");
    spread_dict("@dict::new(x=1, y=2, z=3).", "(x=1, **(y=2, z=3))");
    equals_dict("@dict::new(x=1, y=2, z=3).", "=@dict::new(z=3, y=2, x=1).");
    
    spread_list_wildcard("[1, 2, 3]", "[1, ...]");
    spread_dict_wildcard("@dict::new(x=1, y=2, z=3).", "(x=1, ...)");
    spread_tag_wildcard("<a href=\"foo.html\">foo</a>", "<a ...> _ </a>");
    
    tag_simple("<span>Foo</span>", "<span> _ </span>");
    tag_empty("<span/>", "<span/>");
    tag_attr("<span id=`foobar`>Foo</span>", "<span id=`foobar`> _ </span>");
    tag_wildcard_name("<span>Foo</span>", "<_> _ </>");
    tag_seq("{<a/><b/><i/>}", "{<a/><b/><i/>}");
    tag_spread("{<a/><b/><i/><u/>}", "{<a/> *_ <u/>}");
    tag_one_in_seq("<span/>", "{<span/>}");
    html_text_seq("{Foo}", "{_}");
    html_text_seq_spread("{Foo}", "{*_}");
    
    equals_captured_var("[23, 23]", "[$x, =$x]");
    var_and_literal("[23, 23]", "[23 & $x, 23 & =$x]");
    var_and_template("[`foo bar`, [`foo bar`, `foo`, `bar`]]", "[$x & '$a $b', =[$x, $a, $b]]");
    
    or_left("1", "1 | 2");
    or_right("2", "1 | 2");
    and_or_precedence("1", "1 | 2 & 3");
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
        "@match @dict::new(x=1, y=2, z=3). {(x=$x, **_) -> $x}",
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
    
    or_left_bind(
        "@match 123 {$x | $y -> [$x, $y]}",
        "<ul><li>123</li><li></li></ul>",
    );
    
    or_right_bind(
        "@match 123 {$x: bool | $y -> [$x, $y]}",
        "<ul><li></li><li>123</li></ul>",
    );
}

assert_err! {
    literal_in_html("@match 5 {{5} -> OK}", SyntaxError::PatternCannotMatchHTML);
    no_matching_branch("@match 5 {6 -> Fail}", RuntimeError::NoMatchingBranch);
}
