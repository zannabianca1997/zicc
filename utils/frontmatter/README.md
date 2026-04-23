# `zicc-frontmatter`

Utility crate to parse frontmatter style files

## Usage

Files use `+++` fences with TOML inside:

```text
+++
title = "My file"
draft = false
+++
Actual content goes here.
```

Parse with a typed struct:

```rust
use serde::Deserialize;
use zicc_frontmatter::FrontMatter;

#[derive(Deserialize)]
struct Meta {
    title: String,
    draft: bool,
}

let source = b"+++\ntitle = \"My file\"\ndraft = false\n+++\nActual content.\n";
let parsed = FrontMatter::<Option<Meta>>::parse_optional(source).unwrap();

if let Some(meta) = &parsed.frontmatter {
    println!("{} (draft={})", meta.title, meta.draft);
}
println!("{}", std::str::from_utf8(parsed.content).unwrap());
```

Writing a file with frontmatter:

```rust
use serde::Serialize;

#[derive(Serialize)]
struct Meta {
    title: String,
    draft: bool,
}

let meta = Meta { title: "My file".into(), draft: false };
let mut out = Vec::new();
zicc_frontmatter::write(&meta, &mut out).unwrap();
```
