# Footnotes

Footnotes are written as [keyword-headed text](./text.md#keyword-headed-text),
without initial indentation, where the keyword depends on the footnote's type.

Footnotes may be [referenced as footnotes](./text.md#footnote-references)
within the same [document](./document), in which case they get inserted soon
after their first footnote reference.

Footnotes that are not referenced as such are ignored.
Circular references do not count here (e.g., a footnote referencing itself).


## Footnote text

Footnote text permits [styling](./text.md#styling), but not enumerations.

Note that footnote references within footnotes are permitted.


## Example

```
Some text with a footnote{^:fn} and more text.

^{fn:} The footnote.
```

Example output (assuming footnote keyword `^`):

```
Some text with a footnote¹ and more text.

---
¹ The footnote.
```
