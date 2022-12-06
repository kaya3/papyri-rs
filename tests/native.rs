mod common;

assert_ok! {
    join(
        "@join [Foo, Bar, Baz]",
        "<p>FooBarBaz</p>",
    );
    
    join_with_separator(
        "@join(`, `) [Foo, Bar, Baz]",
        "<p>Foo, Bar, Baz</p>",
    );
    
    join_with_html_separator(
        "@join(<span/>) [Foo, Bar, Baz]",
        "<p>Foo<span></span>Bar<span></span>Baz</p>",
    );
}

assert_matches! {
    filter_none(
        "@filter [1, 2, ., 3, ., 4]",
        "=[1, 2, 3, 4]",
    );
    
    filter_predicate(
        "@filter(@fn $x -> $x) [True, True, False, True]",
        "=[True, True, True]",
    );
    
    map(
        "@map(@fn $x -> [$x, $x]) [1, 2, 3]",
        "=[[1, 1], [2, 2], [3, 3]]",
    );
    
    sorted_ints(
        "@sorted [5, 3, 4, 1, 2]",
        "=[1, 2, 3, 4, 5]",
    );
    
    sorted_strings(
        "@sorted [`foo`, `bar`, `baz`]",
        "=[`bar`, `baz`, `foo`]",
    );
    
    sorted_int_keys(
        "@sorted(key=@fn $p -> @match $p {[$k, _] -> $k}) [[5, True], [3, False], [4, True], [1, False], [2, True]]",
        "=[[1, False], [2, True], [3, False], [4, True], [5, True]]",
    );
    
    sorted_string_keys(
        "@sorted(key=@fn $p -> @match $p {[$k, _] -> $k}) [[`foo`, 1], [`bar`, 2], [`baz`, 3]]",
        "=[[`bar`, 2], [`baz`, 3], [`foo`, 1]]",
    );
    
    sorted_int_as_string(
        "@sorted(key=$str) [1, 3, 22, 15]",
        "=[1, 15, 22, 3]",
    );
    
    sort_reverse(
        "@sorted(reverse=True) [4, 2, 3, 5, 1]",
        "=[5, 4, 3, 2, 1]",
    );
    
    unique_id(
        "[@unique_id foo, @unique_id foo, @unique_id bar, @unique_id foo]",
        "=[`foo`, `foo_2`, `bar`, `foo_3`]"
    );
    
    unique_id_transliterate_unicode(
        "@unique_id `l'Hôpital's rule`",
        "`l_hopital_s_rule`",
    );
}

assert_err! {
    raise("@raise `foobar`", RuntimeError::Raised);
}
