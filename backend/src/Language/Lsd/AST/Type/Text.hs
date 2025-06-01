module Language.Lsd.AST.Type.Text
    ( TextType (..)
    , PreTextType (..)
    , FootnoteType (..)
    , PreFootnoteType (..)
    )
where

import Data.Void (Void)

data TextType enumT
    = TextType
        [enumT]
        [FootnoteType]

data PreTextType enumT
    = PreTextType
        [enumT]
        [PreFootnoteType]

data FootnoteType
    = FootnoteType
        (TextType Void)

data PreFootnoteType
    = PreFootnoteType
        (PreTextType Void)
