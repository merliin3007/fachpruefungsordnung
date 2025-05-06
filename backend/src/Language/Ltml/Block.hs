module Language.Ltml.Block where

import Language.Ltml.Paragraph (Paragraph)
import Language.Ltml.Table (Table)

data Block
  = ParagraphBlock Paragraph
  | TableBlock Table
