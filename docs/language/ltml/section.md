# Sections

Sections generally have an [unstyled-textual](./text.md) heading,
and are composed of lower-level sections, [paragraphs](./block.md),
[tables](todo) or [appendices](todo).

Sections are identified by [keywords](general/identifier.md#input-identifiers),
and not indented.
Compare Markdown's different headings (`#`, `##`),
MediaWiki's sections (`==`, `===`, ...), and
LaTeX' sections (`\section`, `\subsection`, ...).

A section is headed by the keyword in square brackets ("[<keyword>]") or a
symbol keyword, like in the following case "§". The specific keyword is defined in the
respective LSD for the respective section type. A section can optionally be
followed by a label in curly braces. The section text can start directly
thereafter, separated by a space, or in the next line like in this case.

```
§ {sectionA}
{paragraph1:} This paragraph1 in a sectionA.

{paragraphB:} This is another paragraph in "sectionA". Paragraphs don't have
keywords and are just separated by lines.

{tableA:} This will be a table (todo).

{appendixA:} This will be an appendix (todo).
```

Sections can be nested. It could look like this (todo):

```
§ {sectionB}
{
  § {nestedSection}
  {paragraphC:} A simple paragraph.
}

```


