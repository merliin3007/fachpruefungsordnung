module Language.Ltml.AST.Section (Section) where

import Data.Text (Text)
import Language.Lsd.AST.Type.Section (SectionFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Paragraph (Paragraph)

-- sectionKind = Section
-- sectionType = Section SectionFormat
data Section
    = Section
        SectionFormat
        Heading
        [Node SectionChild]

-- TODO: Use PlainText (yet to be defined).
newtype Heading = Heading Text

data SectionChild
    = SectionChildSection Section
    | SectionChildParagraph Paragraph
