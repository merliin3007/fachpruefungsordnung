module Language.Ltml.Paragraph where

import Language.Ltml.Format (IdentifierFormat)
import Language.Ltml.Node (Node)
import Language.Ltml.Sentence (Sentence)

data Paragraph = Paragraph
  ParagraphFormat
  [Node Sentence]

data ParagraphFormat = ParagraphFormat
  IdentifierFormat
