module Language.Ltml.AST.Document
    ( Document (..)
    , DocumentTitle (..)
    , DocumentBody (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Type.Document (DocumentFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Section (Section)
import Language.Ltml.AST.SimpleSection (SimpleSection)

data Document
    = Document
        DocumentFormat
        DocumentTitle
        DocumentBody
    deriving (Show)

newtype DocumentTitle = DocumentTitle Text
    deriving (Show)

data DocumentBody
    = -- | document body
      DocumentBody
        [SimpleSection]
        -- ^ intro
        [Node Section]
        -- ^ main
        [SimpleSection]
        -- ^ outro
    deriving (Show)
