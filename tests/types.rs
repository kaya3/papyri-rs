mod common;

assert_ok! {
    typeof_int(
        "@typeof 23",
        "<p>int</p>",
    );
    
    typeof_str(
        "@typeof `foobar`",
        "<p>str</p>",
    );
    
    typeof_bool(
        "@typeof True",
        "<p>bool</p>",
    );
    
    typeof_none(
        "@typeof .",
        "<p>none</p>",
    );
    
    typeof_int_list(
        "@typeof [1, 2, 3]",
        "<p>int list</p>",
    );
    
    typeof_int_option_list(
        "@typeof [1, ., 2, ., 3]",
        "<p>int? list</p>",
    );
    
    typeof_str_list(
        "@typeof [`foo`, `bar`]",
        "<p>str list</p>",
    );
    
    typeof_int_dict(
        "@typeof @dict::new(x=1, y=2).",
        "<p>int dict</p>",
    );
    
    typeof_str_dict(
        "@typeof @dict::new(x=`foo`, y=`bar`).",
        "<p>str dict</p>",
    );
    
    typeof_bare_str(
        "@typeof foobar",
        "<p>str</p>",
    );
    
    typeof_bare_str_list(
        "@typeof [foo, bar]",
        "<p>str list</p>",
    );
    
    call_optional_str(
        "@fn foo($_0: str?) $_: none -> OK\n@foo(`bar`).",
        "<p>OK</p>",
    );
    
    call_int_list(
        "@fn foo($_0: int list) $_: none -> OK\n@foo([1, 2, 3]).",
        "<p>OK</p>",
    );
    
    call_optional_int_list(
        "@fn foo($_0: int list?) $_: none -> OK\n@foo([1, 2, 3]).",
        "<p>OK</p>",
    );
}