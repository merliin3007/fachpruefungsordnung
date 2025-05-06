module Language.Ltml.Section where

import Data.Text (Text)

import Language.Ltml.Block (Block)
import Language.Ltml.Format (HeadingFormat, IdentifierFormat)
import Language.Ltml.Node (Node)

-- sectionKind = Section
-- sectionType = Section SectionFormat
data Section = Section
  SectionFormat
  Heading
  [Node SectionChild]

data SectionFormat = SectionFormat
  IdentifierFormat
  HeadingFormat

-- TODO: Use PlainText (yet to be defined).
newtype Heading = Heading Text

data SectionChild
  = SubSection Section
  | Block Block
