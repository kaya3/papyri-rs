mod common;

assert_matches! {
    dict_len(
        "@dict::len @dict::new(x=3, y=4).",
        "2",
    );
    
    dict_keys(
        "@dict::keys @dict::new(x=3, y=4).",
        "=[x, y]",
    );
    
    dict_values(
        "@dict::values @dict::new(x=3, y=4).",
        "=[3, 4]",
    );
    
    dict_items(
        "@dict::items @dict::new(x=3, y=4).",
        "=[[x, 3], [y, 4]]",
    );
}
