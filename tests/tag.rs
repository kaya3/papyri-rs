mod common;

assert_ok! {
    simple_tag(
        "<span>Foo</span>",
        "<p><span>Foo</span></p>",
    );
    
    simple_attribute(
        r#"<span id="foo">Foo</span>"#,
        r#"<p><span id="foo">Foo</span></p>"#,
    );
    
    attribute_order(
        r#"<span id="foo" class="foo">Foo</span><span class="bar" id="bar">Bar</span>"#,
        r#"<p><span id="foo" class="foo">Foo</span><span class="bar" id="bar">Bar</span></p>"#,
    );
    
    boolean_attribute(
        "<span hidden>Foo</span>",
        "<p><span hidden>Foo</span></p>",
    );
    
    self_closing(
        "<br>",
        "<p><br></p>",
    );
    
    doctype(
        "<!DOCTYPE html>",
        "<!DOCTYPE html>\n",
    );
    
    auto_close(
        "<span />",
        "<p><span></span></p>",
    );
    
    short_close(
        "<span>Foo</>",
        "<p><span>Foo</span></p>",
    );
    
    block_tag(
        "<div>Foo</div>",
        "<div>Foo</div>",
    );
    
    content_kind(
        "<ul>Foo</ul>",
        "<ul><li>Foo</li></ul>",
    );
    
    content_kind_nested(
        "<table>Foo</table>",
        "<table><tr><td>Foo</td></tr></table>",
    );
    
    var_attribute(
        "@let(id=`foo`) <span id=$id>Foo</span>",
        r#"<p><span id="foo">Foo</span></p>"#,
    );
    
    var_tag_name(
        "@let(t=`span`) <$t>Foo</>",
        "<p><span>Foo</span></p>",
    );
    
    template_attribute(
        r#"@let(c=`foo`) <span class="$c bar">Foo</span>"#,
        r#"<p><span class="foo bar">Foo</span></p>"#,
    );
    
    empty_attribute(
        "@let(id=.) <span id?=$id>Foo</span>",
        "<p><span>Foo</span></p>",
    );
    
    spread_attribute(
        "<span **@dict::new(id=`foo`).>Foo</span>",
        r#"<p><span id="foo">Foo</span></p>"#,
    );
    
    empty_spread_attribute(
        "<span **@dict::new(hidden=.).>Foo</span>",
        r#"<p><span hidden>Foo</span></p>"#,
    );
}

assert_err! {
    unclosed_tag(
        "<span>",
        SyntaxError::TagUnmatchedOpen,
    );
    
    unmatched_closing_tag(
        "</span>",
        SyntaxError::TokenUnmatched,
    );
    
    incorrect_closing_tag(
        "<span></div>",
        SyntaxError::TagUnmatchedOpen,
    );
    
    unclosed_template(
        r#"<span id="foobar>Foobar</span>"#,
        SyntaxError::TokenExpectedWasEOF,
    );
    
    unmatched_self_closing(
        "<br></br>",
        SyntaxError::TokenUnmatched,
    );
    
    duplicate_attribute(
        "<span id=`foo` id=`bar`>Foobar</span>",
        SyntaxError::TagDuplicateAttr,
    );
    
    invalid_tag_name(
        "@let(t=`12345`) <$t>Foobar</>",
        NameError::InvalidTag,
    );
    
    duplicate_attribute_var(
        "<span id=`foo` **@dict::new(id=`bar`).>Foobar</span>",
        RuntimeError::AttrMultipleValues,
    );
    
    self_closing_with_content(
        "@let(tag_name=`img`) <$tag_name>Foo</>",
        TypeError::NoContentAllowed,
    );
}
