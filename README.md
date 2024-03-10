# Scratch Compiler 2

TODO: come up with a name for the language

A compiled programming language that targets Scratch.

For a high-level overview of the compiler, see [the architecture](architecture.md).

Scratch is a project of the Scratch Foundation, in collaboration with the
Lifelong Kindergarten Group at the MIT Media Lab. It is available for free at
<https://scratch.mit.edu>

## Editor support

### Helix

Add the following to `languages.toml`:

```toml
[[language]]
name = "sc2"
scope = "source.sc2"
injection-regex = "sc2"
roots = [".git"]
file-types = ["sc2"]
comment-token = "#"
indent = { tab-width = 4, unit = "    " }
formatter = { command = "/path/to/compiled-executable", args = ["format"] }
auto-format = true

[language.auto-pairs]
'(' = ')'
'{' = '}'
'[' = ']'
'"' = '"'

[[grammar]]
name = "sc2"
source = { path = "/path/to/repo/tree-sitter-sc2" }
```
