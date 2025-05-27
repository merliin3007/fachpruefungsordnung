module Language.Lsd.AST.Type.Paragraph
    ( ParagraphFormat (..)
    , ParagraphType (..)
    , PreParagraphType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Format (IdentifierFormat)
import Language.Lsd.AST.Type.Enum (EnumType)

newtype ParagraphFormat
    = ParagraphFormat
        IdentifierFormat

data ParagraphType
    = ParagraphType
        ParagraphFormat
        [EnumType]

data PreParagraphType
    = PreParagraphType
        ParagraphFormat
        [TypeName]
