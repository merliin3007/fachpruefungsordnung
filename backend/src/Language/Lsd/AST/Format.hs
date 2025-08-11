module Language.Lsd.AST.Format
    ( FormatString (..)
    , FormatAtom (..)
    , IdentifierFormat
    , EnumStyle (..)
    , HeadingFormat
    , HeadingPlaceholderAtom (..)
    , KeyFormat
    , KeyPlaceholderAtom (..)
    , TocKeyFormat (..)
    , EnumItemKeyFormat (..)
    , ParagraphKeyFormat (..)
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

type KeyFormat = FormatString KeyPlaceholderAtom

data KeyPlaceholderAtom = KeyIdentifierPlaceholder
    deriving (Show)

newtype TocKeyFormat = TocKeyFormat KeyFormat
    deriving (Show)

newtype EnumItemKeyFormat = EnumItemKeyFormat KeyFormat
    deriving (Show)

newtype ParagraphKeyFormat = ParagraphKeyFormat KeyFormat
    deriving (Show)
