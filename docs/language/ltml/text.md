# Text

Some nodes contain *text*, which may be [styled](#styling), and contain
[references](#references), [lists](#lists) and [footnotes](todo).

Text may be spread over several lines; linebreaks are translated to single
ASCII space characters.
Empty lines are disallowed. They would terminate a text segment and introduce
a new paragraph.

If headed by a node [keyword](general/identifier.md#input-identifier),
text implicitly adds one level of [indentation](general/indentation.md)
(except for sections).
The first line may generally be written right after the keyword, without extra
indentation.
Sentence terminators do not count as keywords in this context.


## Styling
```
* Nodes may permit styled text.
* Specifically, text may be `<*bold>`, `</in italics>`, `<_underlined>` or
  <*a </<_combination> of these>>
* Different style tags may be nested (e.g., `<*bold </and italics/>*>`), but
  not the same (e.g., `<*bold <*again*>*>` is illegal).
* <*Styled text can
  span multiple lines,
    ^ contain footnotes,
    # contain items
  and end in the next line.>
```

## References

[Labeled](general/label.md) nodes may be referenced within text, using the
respective labels.
In the output, the respective output [identifier](general/identifier.md) is
printed. Ltml can calculate the exact position of the referenced object thanks
to internal numbering of nodes and sentences.

### Example: How to use a reference 
```
{label1:} This is a labelled sentence.

This is a reference of the sentence {:label1}
```

### Example output

This is a labelled sentence.

This is a reference of the sentence § 3 Section 2 Sentence 1
 

## Child nodes

Text nodes (e.g., headings, sentences) generally permit in-line children.
That is, at any point in such a text node, certain children nodes may be
inserted.
This requires breaking the line where the children are to be inserted.

Text children must be [indented](general/indentation.md) one level from the
current context (text);
that is, two levels from the textual node's keyword, if any.


### Lists

One kind of text children are list items.

Example:

```
This is a list with items:
  # item 1
  # item 2
```

See [lists](list.md) for more information and special cases.


### Footnotes

Example:
```
A sentence can have footnotes
  ^ This is a footnote.
and end in the next line.
```

