module Language.Lsd.AST.Format
    ( FormatString (..)
    , FormatAtom (..)
    , IdentifierFormat
    , EnumStyle (..)
    , HeadingFormat
    , HeadingPlaceholderAtom (..)
    )
where

newtype FormatString a = FormatString [FormatAtom a]

data FormatAtom a
    = StringAtom String
    | PlaceholderAtom a

type IdentifierFormat = FormatString EnumStyle

data EnumStyle
    = Arabic
    | AlphabeticLower
    | AlphabeticUpper

type HeadingFormat = FormatString HeadingPlaceholderAtom

data HeadingPlaceholderAtom
    = IdentifierPlaceholder
    | HeadingTextPlaceholder
