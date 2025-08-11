module Language.Lsd.AST.Format
    ( FormatString (..)
    , FormatAtom (..)
    , IdentifierFormat
    , EnumStyle (..)
    , HeadingFormat
    , HeadingPlaceholderAtom (..)
    , TocKeyFormat
    , TocKeyPlaceholderAtom (..)
    )
where

newtype FormatString a = FormatString [FormatAtom a]
    deriving (Show)

data FormatAtom a
    = StringAtom String
    | PlaceholderAtom a
    deriving (Show)

type IdentifierFormat = FormatString EnumStyle

data EnumStyle
    = Arabic
    | AlphabeticLower
    | AlphabeticUpper
    deriving (Show)

type HeadingFormat = FormatString HeadingPlaceholderAtom

data HeadingPlaceholderAtom
    = IdentifierPlaceholder
    | HeadingTextPlaceholder
    deriving (Show)

type TocKeyFormat = FormatString TocKeyPlaceholderAtom

data TocKeyPlaceholderAtom = TocKeyIdentifierPlaceholder
    deriving (Show)
