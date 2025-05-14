module Language.Ltml.AST.Section where

import Data.Text (Text)
import Language.Ltml.AST.Block (Block)
import Language.Ltml.AST.Format (HeadingFormat, IdentifierFormat)
import Language.Ltml.AST.Node (Node)

-- sectionKind = Section
-- sectionType = Section SectionFormat
data Section
    = Section
        SectionFormat
        Heading
        [Node SectionChild]

data SectionFormat
    = SectionFormat
        IdentifierFormat
        HeadingFormat

-- TODO: Use PlainText (yet to be defined).
newtype Heading = Heading Text

data SectionChild
    = SubSection Section
    | Block Block
