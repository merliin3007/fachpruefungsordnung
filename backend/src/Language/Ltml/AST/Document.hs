module Language.Ltml.AST.Document
    ( Document (..)
    , DocumentHeader (..)
    , DocumentBody (..)
    )
where

import Language.Lsd.AST.Type.Document (DocumentFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Section (Section)

data Document
    = Document
        DocumentFormat
        DocumentHeader
        DocumentBody

data DocumentHeader = DocumentHeader

newtype DocumentBody = DocumentBody [Node Section]
