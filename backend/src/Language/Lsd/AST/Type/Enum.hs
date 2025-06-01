module Language.Lsd.AST.Type.Enum
    ( EnumType (..)
    , PreEnumType (..)
    )
where

import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Type.Text (TextType (..))

data EnumType
    = EnumType
        Keyword
        (TextType EnumType)

data PreEnumType
    = PreEnumType
        Keyword
        (TextType TypeName)
