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
    
    str_len(
        "@let(foo=`foobar`) $foo::len",
        "6",
    );
    
    str_is_empty(
        "@let(foo='') $foo::is_empty",
        "True",
    );
    
    str_is_not_empty(
        "@let(foo=`foobar`) $foo::is_empty",
        "False",
    );
    
    list_len(
        "@let(foo=[2, 4, 6]) $foo::len",
        "3",
    );
    
    list_is_empty(
        "@let(foo=[]) $foo::is_empty",
        "True",
    );
    
    list_is_not_empty(
        "@let(foo=[2, 4, 6]) $foo::is_empty",
        "False",
    );
    
    function_name(
        "@let(bar=@fn foo $x -> $x) $bar::name",
        "`foo`",
    );
}
