module Language.Ltml.Document where

import Language.Ltml.Node (Node)
import Language.Ltml.Section (Section)

data Document = Document
  DocumentFormat
  DocumentHeader
  DocumentBody

data DocumentFormat = DocumentFormat

data DocumentHeader = DocumentHeader

data DocumentBody = DocumentBody [Node Section]
