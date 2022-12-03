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
    
    unique_id(
        "@unique_id `foo` @unique_id `foo` @unique_id `bar` @unique_id `foo`",
        "<p>foo foo_2 bar foo_3</p>"
    );
    
    unique_id_transliterate_unicode(
        "@unique_id `l'Hôpital's rule`",
        "<p>l_hopital_s_rule</p>",
    );
}
