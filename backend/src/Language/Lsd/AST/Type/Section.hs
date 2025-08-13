module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , PreSectionType (..)
    , HeadingType (..)
    , PreHeadingType (..)
    , SectionBodyType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Format
    ( HeadingFormat
    , IdentifierFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphType)
import Language.Lsd.AST.Type.Text (PreTextType, TextType)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        TocKeyFormat
    deriving (Show)

data SectionType
    = SectionType
        Keyword
        HeadingType
        SectionFormat
        SectionBodyType

data PreSectionType
    = PreSectionType
        Keyword
        PreHeadingType
        SectionFormat
        TypeName

data HeadingType
    = HeadingType
        HeadingFormat
        (TextType Void)

data PreHeadingType
    = PreHeadingType
        HeadingFormat
        (PreTextType Void)

data SectionBodyType
    = InnerSectionBodyType SectionType
    | LeafSectionBodyType ParagraphType
    | SimpleLeafSectionBodyType SimpleParagraphType
