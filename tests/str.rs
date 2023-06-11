mod common;

assert_matches! {
    escaped_template(
        "\"\\#\"",
        "`#`",
    );
    
    len(
        "@let(foo=`foobar`) @foo::len.",
        "6",
    );
    
    is_empty(
        "@let(foo='') @foo::is_empty.",
        "True",
    );
    
    is_not_empty(
        "@let(foo=`foobar`) @foo::is_empty.",
        "False",
    );
    
    starts_with(
        "@str::starts_with(`foobar`, `foo`).",
        "True",
    );
    
    not_starts_with(
        "@str::starts_with(`foobar`, `bar`).",
        "False",
    );
    
    ends_with(
        "@str::ends_with(`foobar`, `bar`).",
        "True",
    );
    
    not_ends_with(
        "@str::ends_with(`foobar`, `foo`).",
        "False",
    );
    
    split(
        "@str::split(` `) `foo bar`",
        "=[`foo`, `bar`]",
    );
    
    trim(
        "@str::trim `  foo bar   `",
        "`foo bar`",
    );
    
    lower(
        "@str::lower `FOO`",
        "`foo`",
    );
    
    upper(
        "@str::upper `foo`",
        "`FOO`",
    );
    
    parse_int(
        "@int::parse `123`",
        "123",
    );
}

assert_err! {
    parse_int_fail(
        "@int::parse `foobar`",
        RuntimeError::ParseIntError,
    );
}
