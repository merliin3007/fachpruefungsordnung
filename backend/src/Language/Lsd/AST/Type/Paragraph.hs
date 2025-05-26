module Language.Lsd.AST.Type.Paragraph
    ( ParagraphFormat
    )
where

import Language.Lsd.AST.Format (IdentifierFormat)

newtype ParagraphFormat
    = ParagraphFormat
        IdentifierFormat
