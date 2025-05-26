module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , PreSectionType (..)
    , SectionChildType (..)
    )
where

import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Format (HeadingFormat, IdentifierFormat)
import Language.Lsd.AST.SimpleRegex (SimpleRegex)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        HeadingFormat

data SectionType
    = SectionType
        Keyword
        SectionFormat
        (SimpleRegex SectionChildType)

data PreSectionType
    = PreSectionType
        Keyword
        SectionFormat
        (SimpleRegex TypeName)

data SectionChildType
    = SectionChildSectionType SectionType
    | SectionChildParagraphType ParagraphType
