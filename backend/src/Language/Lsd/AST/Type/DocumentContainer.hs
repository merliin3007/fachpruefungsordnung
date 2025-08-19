module Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (..)
    , DocumentContainerType (..)
    , PreDocumentContainerType (..)
    , HeaderFooterFormat (..)
    , HeaderFooterItemFormat (..)
    , HeaderFooterFormatAtom (..)
    )
where

import Data.Typography (FontSize, FontStyle)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (Sequence)
import Language.Lsd.AST.Format (FormatString)
import Language.Lsd.AST.Type.AppendixSection (AppendixSectionType)
import Language.Lsd.AST.Type.Document (DocumentType)

data DocumentContainerFormat
    = -- | format
      DocumentContainerFormat
        HeaderFooterFormat -- ^ header format
        HeaderFooterFormat -- ^ footer format
    deriving (Show)

data DocumentContainerType
    = DocumentContainerType
        DocumentContainerFormat
        DocumentType
        (Sequence AppendixSectionType)

data PreDocumentContainerType
    = PreDocumentContainerType
        DocumentContainerFormat
        TypeName
        (Sequence TypeName)

-- | The format of a printed header/footer.
data HeaderFooterFormat
    = -- | format
      HeaderFooterFormat
        [HeaderFooterItemFormat] -- ^ left
        [HeaderFooterItemFormat] -- ^ middle
        [HeaderFooterItemFormat] -- ^ right
    deriving (Show)

data HeaderFooterItemFormat
    = HeaderFooterItemFormat
        FontSize
        [FontStyle]
        (FormatString HeaderFooterFormatAtom)
    deriving (Show)

data HeaderFooterFormatAtom
    = HeaderFooterSuperTitleAtom
    | HeaderFooterTitleAtom
    | HeaderFooterDateAtom
    | HeaderFooterCurPageNumAtom
    | HeaderFooterLastPageNumAtom
    deriving (Show)
