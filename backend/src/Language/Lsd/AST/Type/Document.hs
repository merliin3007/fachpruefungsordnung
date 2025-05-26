module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , DocumentType (..)
    , PreDocumentType (..)
    )
where

import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.SimpleRegex (SimpleRegex)
import Language.Lsd.AST.Type.Header (HeaderNodeType)
import Language.Lsd.AST.Type.Section (SectionType)

data DocumentFormat = DocumentFormat

data DocumentType
    = DocumentType
        Keyword
        DocumentFormat
        [HeaderNodeType]
        (SimpleRegex SectionType)

data PreDocumentType
    = PreDocumentType
        Keyword
        DocumentFormat
        [TypeName]
        (SimpleRegex TypeName)
