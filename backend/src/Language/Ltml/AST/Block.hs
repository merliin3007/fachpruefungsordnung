module Language.Ltml.AST.Block (Block) where

import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.Table (Table)

data Block
    = ParagraphBlock Paragraph
    | TableBlock Table
