module Language.Ltml.AST.Section
    ( Section (..)
    , Heading (..)
    )
where

import Language.Lsd.AST.Format (HeadingFormat)
import Language.Lsd.AST.Type.Section (SectionFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.Text (PlainTextTree)

data Section
    = Section
        SectionFormat
        Heading
        -- | children
        (Either [Node Paragraph] [Node Section])
    deriving (Show)

data Heading
    = Heading
        HeadingFormat
        [PlainTextTree]
    deriving (Show)
