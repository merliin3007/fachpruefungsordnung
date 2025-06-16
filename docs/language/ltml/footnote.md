# Footnotes

Footnotes may occur anywhere within [text](./text.md), and are written as
[text children](./text.md#child-nodes), headed by a keyword that is specific
to the footnote type.


## Footnote text

Footnote text permits [styling](./text.md#styling), but not enumerations.

Note that footnotes within footnotes are permitted, because footnotes are
permitted in any text.


## Example

```
Some text with
  ^ a footnote
and more text.
```

Example output (assuming footnote keyword `^`):

```
Some text with¹ and more text.

---
¹ a footnote
```
