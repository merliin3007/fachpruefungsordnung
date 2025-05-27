module Language.Lsd.AST.Type.Enum
    ( EnumType (..)
    , PreEnumType (..)
    )
where

import Language.Lsd.AST.Common (Keyword, TypeName)

data EnumType
    = EnumType
        Keyword
        [EnumType]

data PreEnumType
    = PreEnumType
        Keyword
        [TypeName]
