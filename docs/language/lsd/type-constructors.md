# Type constructors

* This is semi-formal, and to be read together with the
  [EBNF grammar](syntax.ebnf.txt).
* A type constructor header is to be read as
  `type/KindName constructor TypeConstructorName`
* `format-string(A, B, ...)` matches strings that may contain `{A}`, `{B}`,
  ...
* `[type/Kind]` matches comma-separated lists of defined types of kind `Kind`.
* `regexp([type/Kind])` matches simple regular expressions built from defined
  types of kind `Kind` as atoms and combined using ` `, `|`, `?`, `*`, `+`,
  `()`.


## Document type constructors

There is only one type constructor for the document kind.

```
type/document constructor document:
  keyword: NodeKeyword
  header_nodes: [type/header-node]
  body_nodes: regexp([type/body-node])
```


## Header node type constructors

*none* (TODO)


## Body node type constructors

```
type/body-node constructor section:
  keyword: NodeKeyword
  has_title: Bool
  output_identifier_format: format-string("arabic", "alph", "Alph")
  output_heading_location: "top-center"|"left-top"
  output_heading_format: format-string("identifier")
  heading_line_children: regexp([type/body-node])
  children: regexp([type/body-node])
  permit_label: Bool
  identifiers_are_fixed: Bool

type/body-node constructor text:
  keyword: NodeKeyword
  output_identifier_format: format-string("arabic", "alph", "Alph")
  line_children: regexp([type/body-node])
  permit_label: Bool
  identifiers_are_fixed: Bool

type/body-node constructor text-block:
  keyword: NodeKeyword
  output_identifier_format: format-string("arabic", "alph", "Alph")
  line_children: regexp([type/body-node])
  permit_label: Bool
  identifiers_are_fixed: Bool

type/body-node constructor text-list:
  item_keyword: NodeKeyword
  item_output_identifier_format: format-string("arabic", "alph", "Alph")
  item_output_key_format: format-string("identifier")
  item_line_children: regexp([type/body-node])
  item_permit_label: Bool
  item_identifiers_are_fixed: Bool

type/body-node constructor footnote:
  keyword: NodeKeyword
  has_title: Bool
  output_identifier_format: format-string("arabic", "alph", "Alph")

type/body-node constructor document-list:
  keyword: NodeKeyword
  documents: [type/document]
```
