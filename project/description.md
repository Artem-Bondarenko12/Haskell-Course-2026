# RegexEngine

A small project for parsing regular expressions into an abstract syntax tree.

## Supported syntax

- Literal characters, for example `a`
- Concatenation, for example `ab`
- Alternation with `|`, for example `a|b`
- Kleene star with `*`, for example `a*`
- Grouping with parentheses, for example `(a|b)*c`
- Empty expression, printed as `ε`
- Escaped special characters, for example `a\*b`

## Error reporting

The parser reports syntax errors with a column number and a readable message. Examples include dangling `*`, trailing backslash escapes, and mismatched parentheses.

## Project structure

```text
project/
  README.md
  regex-engine.cabal
  src/
    Main.hs
```

## How to run

From the `project` directory:

```bash
cabal build
cabal run regex-engine
```

The program prints demo parses and then runs the built-in parser tests.

Expected final output line:

```text
All parser tests passed.
```
