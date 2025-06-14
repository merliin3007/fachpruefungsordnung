# Paragraphs

Paragraphs are composed of text.

Unlike sections, paragraphs are not introduced by keywords. They are text
segments within a section, separated by empty lines.

```
This is a simple paragraph.

{labelForReferencingThisParagraph:}
This is another paragraph. In case that the paragraph is labelled, the
paragraph starts with its label with a ":" in curly braces and text starts
directly thereafter, separated by a space, or in the next line like in this case.

```

A paragraph text can have all formatting features of ltml [text](text.md):
- styled text
- list items
- footnotes 
- sentence numbering
- reference labels

The paragraph in the following example uses them.

```
{paragraphA:} This paragraph uses all formatting features in a minimalistic
way.
Some of this text is <*bold>, </cursive> or <_underlined>.
This is a list:
  # list item 1
  # list item 2
    # nested list item
Next, we use a footnote.
  ^ This is a footnote.
{sentenceA:} This sentence has a label.
This is a reference of "sentenceA": {:sentenceA}.

```
Referencing of sentences is possible thanks to sentence numbering of Ltml.
Sentences are generally terminated by a single period (`.`), each. There are
exceptions of sentence numbering.

Please see [text](text.md) for more information and special cases.

