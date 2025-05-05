# Identifiers

Nodes may have input and output identifiers, which are generally independent.


## Input identifiers

Input identifiers (also: *keywords*) are only visible in the input.

Examples: `#`, `§`, `[intro]`.


## Output identifiers

Output identifiers are automatically generated, and inserted in appropriate
places in the output.
Typically, they are printed alongside the respective nodes.
They are also printed in place of [references](../text.md#references).

Examples: `42`, `a`, `42a`


### Fixed identifiers

Nodes may have their output identifiers fixed upon publication, in which case
such a node cannot be removed, but only marked as removed (`-` suffix), and
further such nodes can only be inserted with the special `*` suffix on the
section keyword.
