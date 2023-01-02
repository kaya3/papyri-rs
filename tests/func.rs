mod common;

assert_ok! {
    simple_function(
        "@b Something",
        "<p><b>Something</b></p>",
    );
    
    positional_argument(
        "@href(`foo.html`) Foo",
        r#"<p><a href="foo.html">Foo</a></p>"#,
    );
    
    named_argument(
        "@image(alt=`Foo`) `foo.png`",
        r#"<img src="foo.png" alt="Foo">"#,
    );
    
    optional_argument(
        "@image `foo.png`",
        r#"<img src="foo.png">"#,
    );
    
    group_function(
        "@b {Something else}",
        "<p><b>Something else</b></p>",
    );
    
    ellipsis_contents(
        "@b ... Hello, world!",
        "<p><b>Hello, world!</b></p>",
    );
    
    ellipsis_in_group(
        "{@b ... Hello, world!} Oh.",
        "<p><b>Hello, world!</b> Oh.</p>",
    );
    
    ellipsis_in_tag(
        "<span>@b ... Hello, world!</span> Oh.",
        "<p><span><b>Hello, world!</b></span> Oh.</p>",
    );
    
    pos_spread(
        "@fn foo(*$_v) $_ -> $_v\n@foo(1, 2, 3).",
        "<ul><li>1</li><li>2</li><li>3</li></ul>",
    );
    
    add_as_method(
        "@let(x=1) @x::add(1).",
        "<p>2</p>",
    );
    
    join_as_method(
        "@let(foo=[1, 2, 3]) @foo::join(`, `).",
        "<p>1, 2, 3</p>"
    );
    
    call_attr_function(
        "@let(foo=@dict::new(bar=@fn $_ -> 23).) @foo::bar.",
        "<p>23</p>",
    );
    
    empty_content_param(
        "@fn foo . -> Foo\n@foo.",
        "<p>Foo</p>",
    );
}

assert_matches! {
    call_if_not_unit(
        "@let(foo=.) @foo?::bar.",
        ".",
    );
    
    function_name(
        "@let(bar=@fn foo $x -> $x) @bar::name.",
        "`foo`",
    );
    
    bind_none(
        "@let(foo=@function::bind(@fn $_ -> 4).) @foo.",
        "4",
    );
    
    bind_pos(
        "@let(foo=@function::bind(@fn($_x, $_y) $_ -> [$_x, $_y], 1).) @foo(2).",
        "=[1, 2]",
    );
    
    bind_named(
        "@let(foo=@function::bind(@fn($x, $y) $_ -> [$x, $y], x=1).) @foo(y=2).",
        "=[1, 2]",
    );
    
    bind_pos_spread(
        "@let(foo=@function::bind(@fn(*$_v) $_ -> $_v, 1, 2).) @foo(3, 4).",
        "=[1, 2, 3, 4]",
    );
    
    bind_named_spread(
        "@let(foo=@function::bind(@fn(**$kw) $_ -> $kw, x=1, y=2).) @foo(z=3).",
        "(x=1, y=2, z=3)",
    );
    
    bind_content(
        "@let(foo=@function::bind(@fn $v: int -> $v) 23) @foo.",
        "23",
    );
    
    bind_as_method(
        "@let(foo=@fn($x, $y) $_ -> [$x, $y], foo_bound=@foo::bind(x=1).) @foo_bound(y=2).",
        "=[1, 2]",
    );
}
