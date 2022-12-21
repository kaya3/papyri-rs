mod common;

assert_matches! {
    compile(
        "@regex::compile `.*`",
        "_: regex",
    );
    
    whole_string(
        "@regex::find(@regex::compile `.*`) `foo bar`",
        "`foo bar`",
    );
    
    char_class(
        "@regex::find(@regex::compile `[a-z]*`) `foo bar`",
        "`foo`",
    );
    
    char_class_all(
        "@regex::find_all(@regex::compile `[a-z]*`) `foo bar`",
        "=[`foo`, `bar`]",
    );
    
    capture_groups(
        "@regex::find(@regex::compile `([a-z]+)([0-9]+)`) `foo123 bar456`",
        "=[`foo123`, `foo`, `123`]",
    );
    
    capture_groups_all(
        "@regex::find_all(@regex::compile `([a-z]+)([0-9]+)`) `foo123 bar456`",
        "=[[`foo123`, `foo`, `123`], [`bar456`, `bar`, `456`]]",
    );
    
    named_capture_groups(
        "@regex::find(@regex::compile `(?P<letters>[a-z]+)(?P<numbers>[0-9]+)`) `foo123 bar456`",
        "(letters=`foo`, numbers=`123`)",
    );
    
    named_capture_groups_all(
        "@regex::find_all(@regex::compile `(?P<letters>[a-z]+)(?P<numbers>[0-9]+)`) `foo123 bar456`",
        "[(letters=`foo`, numbers=`123`), (letters=`bar`, numbers=`456`)]",
    );
    
    trim(
        "@str::trim `  foo  `",
        "`foo`",
    );
    
    no_trim(
        "@str::trim `foo`",
        "`foo`",
    );
}
