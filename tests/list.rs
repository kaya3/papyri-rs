mod common;

assert_ok! {
    basic_ul(
        "[Hello, world]",
        "<ul><li>Hello</li><li>world</li></ul>",
    );
    
    basic_ol(
        "@numbered [Hello, world]",
        "<ol><li>Hello</li><li>world</li></ol>",
    );
    
    nested(
        "[Hello, [Papyri, world]]",
        "<ul><li>Hello</li><li><ul><li>Papyri</li><li>world</li></ul></li></ul>",
    );
    
    spread(
        "[Hello, *[Papyri, world]]",
        "<ul><li>Hello</li><li>Papyri</li><li>world</li></ul>",
    );
    
    join(
        "@list::join [Foo, Bar, Baz]",
        "<p>FooBarBaz</p>",
    );
    
    join_with_separator(
        "@list::join(`, `) [Foo, Bar, Baz]",
        "<p>Foo, Bar, Baz</p>",
    );
    
    join_with_html_separator(
        "@list::join(<span/>) [Foo, Bar, Baz]",
        "<p>Foo<span></span>Bar<span></span>Baz</p>",
    );
}

assert_matches! {
    filter_none(
        "@list::filter [1, 2, ., 3, ., 4]",
        "=[1, 2, 3, 4]",
    );
    
    filter_predicate(
        "@list::filter(@fn $x -> $x) [True, True, False, True]",
        "=[True, True, True]",
    );
    
    map(
        "@list::map(@fn $x -> [$x, $x]) [1, 2, 3]",
        "=[[1, 1], [2, 2], [3, 3]]",
    );
    
    slice(
        "@list::slice(2, 4) [0, 1, 2, 3, 4, 5]",
        "=[2, 3]",
    );
    
    slice_negative(
        "@list::slice(-2) [0, 1, 2, 3, 4, 5]",
        "=[4, 5]",
    );
    
    slice_empty(
        "@list::slice(2, 0) [0, 1, 2, 3, 4, 5]",
        "=[]",
    );
    
    sorted_ints(
        "@list::sorted [5, 3, 4, 1, 2]",
        "=[1, 2, 3, 4, 5]",
    );
    
    sorted_strings(
        "@list::sorted [`foo`, `bar`, `baz`]",
        "=[`bar`, `baz`, `foo`]",
    );
    
    sorted_int_keys(
        "@list::sorted(key=@fn $p -> @match $p {[$k, _] -> $k}) [[5, True], [3, False], [4, True], [1, False], [2, True]]",
        "=[[1, False], [2, True], [3, False], [4, True], [5, True]]",
    );
    
    sorted_string_keys(
        "@list::sorted(key=@fn $p -> @match $p {[$k, _] -> $k}) [[`foo`, 1], [`bar`, 2], [`baz`, 3]]",
        "=[[`bar`, 2], [`baz`, 3], [`foo`, 1]]",
    );
    
    sorted_int_as_string(
        "@list::sorted(key=$str::from) [1, 3, 22, 15]",
        "=[1, 15, 22, 3]",
    );
    
    sort_reversed(
        "@list::sorted(reversed=True) [4, 2, 3, 5, 1]",
        "=[5, 4, 3, 2, 1]",
    );
}
