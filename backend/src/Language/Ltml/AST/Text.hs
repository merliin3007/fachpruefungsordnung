{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.AST.Text
    ( TextTree (..)
    , PlainTextTree
    , FootnoteTextTree
    , RichTextTree
    , ParagraphTextTree
    , FontStyle (..)
    , EnumItem (..)
    , SentenceStart (..)
    )
where

import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Data.Void (Void)
import Language.Ltml.AST.Label (Label)

data TextTree style enumItem special
    = TextLeaf Text
    | Special special
    | Reference Label
    | Styled style [TextTree style enumItem special]
    | EnumChild enumItem
    | Footnote [FootnoteTextTree]
    deriving (Show)

instance FromWhitespace (TextTree a b c) where
    fromWhitespace "" = TextLeaf ""
    fromWhitespace _ = TextLeaf " "

type PlainTextTree = TextTree Void Void Void

type FootnoteTextTree = TextTree FontStyle Void Void

type RichTextTree = TextTree FontStyle EnumItem Void

type ParagraphTextTree = TextTree FontStyle EnumItem SentenceStart

data FontStyle
    = Bold
    | Italics
    | Underlined
    deriving (Show)

newtype EnumItem = EnumItem [TextTree FontStyle EnumItem Void]
    deriving (Show)

newtype SentenceStart = SentenceStart (Maybe Label)
    deriving (Show)
