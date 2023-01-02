mod common;

assert_matches! {
    attr_access(
        "@let(foo=@dict::new(x=23).) $foo::x",
        "23",
    );
    
    attr_access_nested(
        "@let(foo=@dict::new(bar=@dict::new(x=23).).) $foo::bar::x",
        "23",
    );
    
    attr_access_coalesce(
        "@let(foo=.) $foo?::x",
        ".",
    );
    
    dict_len(
        "@dict::len @dict::new(x=3, y=4).",
        "2",
    );
    
    dict_keys(
        "@dict::keys @dict::new(x=3, y=4).",
        "=[x, y]",
    );
    
    dict_values(
        "@dict::values @dict::new(x=3, y=4).",
        "=[3, 4]",
    );
    
    dict_items(
        "@dict::items @dict::new(x=3, y=4).",
        "=[[x, 3], [y, 4]]",
    );
}
