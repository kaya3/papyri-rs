mod common;

assert_ok! {
    nbsp("~", "<p>\u{a0}</p>");
    dashes("-- ---", "<p>– —</p>");
    arrows("<- -> <->", "<p>← → ↔</p>");
    other_subs("<= >= != ... +/- (c)", "<p>≤ ≥ ≠ … ± ©</p>");
    negative_number("-5", "<p>−5</p>");
    en_dash_range("5--8", "<p>5–8</p>");
    
    escape(r"\$ \@", "<p>$ @</p>");
    hex_escape(r"\x41 \u0041 \U00000041", "<p>A A A</p>");
    html_entities("&quot; &#65; &#x41;", "<p>\" A A</p>");
    
    single_quotes("I'm 'ok' now", "<p>I’m ‘ok’ now</p>");
    double_quotes(r#"You "are" now"#, "<p>You “are” now</p>");
    quote_override(r#"{}'single{'}, {}"double{"} "#, "<p>’single‘, ”double“</p>");
    
    escape_html(r"\<span\>", "<p>&lt;span&gt;</p>");
}
