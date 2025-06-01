module Language.Lsd.AST.Type.Text
    ( TextType (..)
    )
where

data TextType enumT
    = TextType
        [enumT]
        --[FootnoteType] -- TODO
