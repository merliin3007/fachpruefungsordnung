

# Lists

Lists are [text children](./text.md#child-nodes).

Lists are composed of [textual](./text.md) items, which are either numbered
sequentually, or unnumbered
(c.f. LaTeX' `enumerate` and `itemize` environments).

List may indirectly contain sub-lists:
the text within lists may have a list as text child.

```
A text can contain lists of items:
  # A child item is introduced by an "#". The symbol "#" is indented two
     spaces more than its relative parent while the following text can have
     another at least two spaces higher, consistent, and user-defined
     indentation (in this case three additional spaces).
  # This item has the same parent like the child item above
     # This item is a child of the second child item
```


