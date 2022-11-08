use aho_corasick::{AhoCorasick, MatchKind, AhoCorasickBuilder};
use once_cell::sync::Lazy;

static TEXT_SUBS: Lazy<AhoCorasick> = Lazy::new(
    || AhoCorasickBuilder::new()
        .match_kind(MatchKind::LeftmostLongest)
        .build(&[
            "---", // em dash
            "--", // en dash
            "<->", // left-right arrow
            "<-", // left arrow
            "->", // right arrow
            "!=", // not equal
            "<=", // less than or equal
            ">=", // greater than or equal
            "+/-", // plus or minus
            "(c)", // copyright
        ])
);

pub fn text_substitutions(s: &str) -> String {
    TEXT_SUBS.replace_all(s, &[
        "\u{2014}", // em dash
        "\u{2013}", // en dash
        "\u{2194}", // left-right arrow
        "\u{2190}", // left arrow
        "\u{2192}", // right arrow
        "\u{2260}", // not equal
        "\u{2264}", // less than or equal
        "\u{2265}", // greater than or equal
        "\u{00B1}", // plus or minus
        "\u{00A9}", // copyright
    ])
}
