pub fn is_ascii_alphabetic(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_alphabetic())
}

pub fn is_whitespace(s: &str) -> bool {
    s.chars().all(char::is_whitespace)
}

pub fn fix_indentation(s: &str) -> String {
    let mut indentation_to_remove: Option<&str> = None;
    let mut out = "".to_string();
    for line in s.trim_end().lines() {
        match indentation_to_remove {
            Some(indentation) => {
                if line.starts_with(indentation) {
                    out += &line[indentation.len()..];
                } else {
                    out += &line.trim_start();
                }
                out += "\n";
            },
            None => {
                if let Some((index, _)) = line.chars().enumerate().find(|(_, c)| !c.is_whitespace()) {
                    indentation_to_remove = Some(&line[..index]);
                    out += &line[index..];
                    out += "\n";
                }
            },
        }
    }
    out
}
