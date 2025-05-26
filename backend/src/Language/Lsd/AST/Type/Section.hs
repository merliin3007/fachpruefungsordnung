module Language.Lsd.AST.Type.Section
    ( SectionFormat
    )
where

import Language.Lsd.AST.Format (HeadingFormat, IdentifierFormat)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        HeadingFormat
