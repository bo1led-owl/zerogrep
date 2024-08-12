# Zerogrep

A `grep` with custom regex engine written in Zig

## Usage
    zg [flags]... PATTERN FILES...

or instead of `FILES` you can redirect `zg`'s input and it will read from stdin.
For more details see `zg --help`

## Supported regex features
- Grouping (`foo(bar)+baz`)
- Repetition (`*` and `+`)
- Optionals (`?`)
- Anchors (`^` and `$`)
- Character groups (`[fobar]`, `[a-zA-Z_]` and similar)
