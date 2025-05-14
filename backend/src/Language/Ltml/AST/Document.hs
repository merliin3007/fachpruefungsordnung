module Language.Ltml.AST.Document where

import           Language.Ltml.AST.Node    (Node)
import           Language.Ltml.AST.Section (Section)

data Document
  = Document
      DocumentFormat
      DocumentHeader
      DocumentBody

data DocumentFormat = DocumentFormat

data DocumentHeader = DocumentHeader

newtype DocumentBody = DocumentBody [Node Section]
