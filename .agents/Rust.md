# General coding guidance

- Do not add dependencies manually. Use `cargo add`. Fallback to manually
  editing `Cargo.toml` only if necessary
- To check your work, remember you are in a workspace. `cargo check
  --workspace`, `cargo test --all`, etc.
- Remember to format your code with `cargo fmt`

# Code organization

- Keep code in the lib, main should be reserved for cli code.
- `use` instructions, unless bound to a specific feature flag, must be at the
  top of the file, ordered in three blocks - std/core, external, workspace deps
- Prefer `<module_name>.rs` to `<module_name>/mod.rs`

# Packaging 

- Packages are prepended with the project name, then hyphen separated path from
  the root. So a package inside `package/path` will be called
  `project-package-path` 
- Package that are only relative to another are inside the other source, sibling
  of the `src` directory
- All packages inherit workspace version and edition
- At the root of every package there is a `README.md` with the package name and
  purpose. This is usually imported into `lib.rs` as `#![doc =
  include_str!("../README.md")]`
- Workspace members are imported by path and never specify a version.

# Dependencies

- All workspace member crates are declared in `[workspace.dependencies]` in the
  root `Cargo.toml`.
- External dependencies used by a **single** crate are declared directly in that
  crate's `Cargo.toml` (no `workspace = true`).
- External dependencies used by **multiple** crates are declared in
  `[workspace.dependencies]` in the root `Cargo.toml`, and individual crates
  reference them with `.workspace = true`.

# Tests

- Tests are placed inside a `tests` module, either in a separated file or inline
  if they are few enough.
- Tests are called with the convention `<subject>_should_<expectation>`
- Tests have a short doc comment explaining what is being tested
