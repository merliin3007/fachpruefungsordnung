# LTML overview

An LTML document is composed of a header and a body, where the header contains
some metadata and the body is a tree.
The permitted header nodes, body nodes and their composability are determined
by an [LTML Schema Definition](../lsd.md).

The syntax is inspired by [Markdown](https://commonmark.org), among others.


## Header nodes

* Title
* Output header (displayed on the top of each page)
* Output foorter (displayed on the bottom of each page)


## Body nodes

### Sections

Sections generally have a heading and content, which may be composed of
lower-level sections or other body nodes.
Sections are identified by keywords (e.g., `§`), and not indented.
Compare Markdown's different headings (`#`, `##`), MediaWiki's sections (`==`,
`===`, ...), and LaTeX' sections (`\section`, `\subsection`, ...).


### Content nodes

Content nodes generally do not have a heading, may contain other content
nodes, and use indentation to determine the tree structure.
Specifically, there are:

* Text blocks (paragraphs)
* Text
* Lists (c.f. LaTeX' `enumerate` and `itemize` environments).
* Footnotes
* Tables


### Appendices

An LTML document may include other LTML documents, as appendices.
These are written separately.


## Common nodes

### Comments

* Generally, to any node, a comment may be added.
* Comments are identified by the `@` keyword.
* Each comment has a signature (author, timestamp).
* If using the web editor, the signature is automatically generated.


### Change suggestions

Within comments, formal change suggestions can be made.


## Labels & references

* Body nodes that have an associated (auto-generated) enumeration identifier
  (e.g., `42` or `a`) may permit or require (depending on the respective
  [LTML Schema Definition](../lsd.md)) adding a label.
* In text, one may refer to labeled nodes using the respective label.
* In the output, the respective enumeration identifier is printed.


## Text formatting

* Nodes may permit formatted text.
* Specifically, text may be `<*bold*>`, `</in italics/>`, and/or
  `<_undelined_>`.
* Different formatting may be nested (e.g., `<*bold </and italics/>*>`), but
  not the same (e.g., `<*bold <*again*>*>` is illegal).


## Fixed identifiers

Nodes may have their output identifiers fixed upon publication, in which case
such a node cannot be removed, but only marked as removed (`-` suffix), and
further such nodes can only be added with the special `*` suffix on the
section keyword.
