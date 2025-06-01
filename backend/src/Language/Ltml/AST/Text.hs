{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Ltml.AST.Text
    ( TextTree (..)
    , PlainTextTree
    , FootnoteTextTree
    , RichTextTree
    , ParagraphTextTree
    , FontStyle (..)
    )
where

import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Language.Ltml.AST.Label (Label)

data TextTree (isRich :: Bool) (hasEnums :: Bool) (isParagraph :: Bool) where
    TextLeaf :: Text -> TextTree r e p
    SentenceStart :: Maybe Label -> TextTree r e 'True
    Reference :: Label -> TextTree r e p
    Styled :: FontStyle -> [TextTree 'True e p] -> TextTree 'True e p
    EnumItem :: [TextTree r 'True 'False] -> TextTree r 'True p
    Footnote :: [TextTree r 'False 'False] -> TextTree r e p

deriving instance Show (TextTree r e p)

instance FromWhitespace (TextTree r e p) where
    fromWhitespace "" = TextLeaf ""
    fromWhitespace _ = TextLeaf " "

type PlainTextTree = TextTree 'False 'False 'False

type FootnoteTextTree = TextTree 'True 'False 'False

type RichTextTree = TextTree 'True 'True 'False

type ParagraphTextTree = TextTree 'True 'True 'True

data FontStyle
    = Bold
    | Italics
    | Underlined
    deriving (Show)
