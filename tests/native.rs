mod common;

assert_matches! {
    add(
        "@int::add(1, 1).",
        "2",
    );
    
    negate(
        "@let(x=4) @x::negate.",
        "-4",
    );
    
    escape_html(
        "@let(x=`<foo>&</foo>`) @x::escape_html.",
        "`&lt;foo&gt;&amp;&lt;/foo&gt;`",
    );
    
    unique_id(
        "[@str::unique_id foo, @str::unique_id foo, @str::unique_id bar, @str::unique_id foo]",
        "=[`foo`, `foo_2`, `bar`, `foo_3`]"
    );
    
    unique_id_transliterate_unicode(
        "@str::unique_id `l'Hôpital's rule`",
        "`l_hopital_s_rule`",
    );
}

assert_err! {
    raise("@raise `foobar`", RuntimeError::Raised);
}
