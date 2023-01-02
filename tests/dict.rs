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
    
    get(
        "@let(foo=@dict::new(x=23)., key=`x`) @dict::get($key) $foo",
        "23",
    );
    
    len(
        "@dict::len @dict::new(x=3, y=4).",
        "2",
    );
    
    keys(
        "@dict::keys @dict::new(x=3, y=4).",
        "=[x, y]",
    );
    
    values(
        "@dict::values @dict::new(x=3, y=4).",
        "=[3, 4]",
    );
    
    items(
        "@dict::items @dict::new(x=3, y=4).",
        "=[[x, 3], [y, 4]]",
    );
}

assert_err! {
    no_such_attr(
        "@let(foo=@dict::new(x=23).) $foo::y",
        NameError::NoSuchAttribute,
    );
    
    get_key_missing(
        "@let(foo=@dict::new(x=23)., key=`y`) @dict::get($key) $foo",
        NameError::NoSuchAttribute,
    );
}
