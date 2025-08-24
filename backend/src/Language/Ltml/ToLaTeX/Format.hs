{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Format
    ( Stylable (..)
    , emptyIdentifierFormat
    , emptyAppendixFormat
    , emptyHeadingFormat
    , formatHeading
    , formatKey
    , staticDocumentFormat
    , getIdentifier
    , getEnumStyle
    , formatHeaderFooterItem
    ) where

import Data.Char (chr)
import qualified Data.Text.Lazy as LT
import Data.Typography
    ( FontSize (..)
    , FontStyle (..)
    , TextAlignment (..)
    , Typography (..)
    )
import Data.Void (Void, absurd)
import Language.Lsd.AST.Format
    ( EnumStyle (AlphabeticLower, AlphabeticUpper, Arabic)
    , FormatAtom (PlaceholderAtom, StringAtom)
    , FormatString (..)
    , HeadingFormat (..)
    , HeadingPlaceholderAtom (..)
    , IdentifierFormat
    , KeyFormat
    , KeyPlaceholderAtom (KeyIdentifierPlaceholder)
    , TocKeyFormat (..)
    )
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixElementFormat (AppendixElementFormat)
    )
import Language.Lsd.AST.Type.DocumentContainer
    ( HeaderFooterFormatAtom (..)
    , HeaderFooterItemFormat (HeaderFooterItemFormat)
    )
import Language.Ltml.ToLaTeX.Type
    ( LaTeX (Raw, Sequence, Text)
    , bold
    , center
    , documentclass
    , flushleft
    , flushright
    , italic
    , large
    , linebreak
    , setfontArabic
    , small
    , underline
    , usepackage
    )

class Stylable a where
    applyTextStyle :: a -> LaTeX -> LaTeX

instance Stylable Void where
    applyTextStyle = absurd

instance Stylable FontStyle where
    applyTextStyle Bold = bold
    applyTextStyle Italics = italic
    applyTextStyle Underlined = underline

instance Stylable TextAlignment where
    applyTextStyle LeftAligned = flushleft . (: [])
    applyTextStyle Centered = center . (: [])
    applyTextStyle RightAligned = flushright . (: [])

instance Stylable FontSize where
    applyTextStyle SmallFontSize = small
    applyTextStyle MediumFontSize = id
    applyTextStyle LargeFontSize = large

instance Stylable Typography where
    applyTextStyle (Typography alignment fontsize styles) =
        applyTextStyle alignment
            . applyTextStyle fontsize
            . foldr (\style acc -> acc . applyTextStyle style) id styles

emptyIdentifierFormat :: IdentifierFormat
emptyIdentifierFormat = FormatString []

emptyKeyFormat :: KeyFormat
emptyKeyFormat = FormatString []

emptyHeadingFormat :: HeadingFormat b
emptyHeadingFormat = HeadingFormat (Typography LeftAligned MediumFontSize []) (FormatString [])

emptyTocKeyFormat :: TocKeyFormat
emptyTocKeyFormat = TocKeyFormat emptyKeyFormat

emptyAppendixFormat :: AppendixElementFormat
emptyAppendixFormat =
    AppendixElementFormat emptyIdentifierFormat emptyTocKeyFormat emptyHeadingFormat

formatHeading
    :: FormatString (HeadingPlaceholderAtom b) -> LaTeX -> LaTeX -> LaTeX
formatHeading (FormatString []) _ _ = mempty
formatHeading (FormatString (StringAtom s : rest)) i latex =
    Sequence (map replace s) <> formatHeading (FormatString rest) i latex
  where
    replace '\n' = linebreak
    replace c = Text (LT.pack [c])
formatHeading (FormatString (PlaceholderAtom a : rest)) i latex =
    case a of
        HeadingTextPlaceholder -> latex <> formatHeading (FormatString rest) i latex
        IdentifierPlaceholder -> i <> formatHeading (FormatString rest) i latex

formatKey :: KeyFormat -> LaTeX -> LaTeX
formatKey (FormatString []) _ = mempty
formatKey (FormatString (StringAtom s : rest)) n =
    Text (LT.pack s) <> formatKey (FormatString rest) n
formatKey (FormatString (PlaceholderAtom KeyIdentifierPlaceholder : rest)) n =
    n <> formatKey (FormatString rest) n

staticDocumentFormat :: LaTeX
staticDocumentFormat =
    Sequence
        [ documentclass [] "article"
        , setfontArabic
        , usepackage
            [ "letterpaper"
            , "top=2cm"
            , "bottom=2cm"
            , "left=3cm"
            , "right=3cm"
            , "marginparwidth=1.75cm"
            ]
            "geometry"
        , usepackage
            [ "colorlinks=true"
            , "allcolors=red"
            ]
            "hyperref"
        , usepackage [] "enumitem"
        , usepackage [] "tabularx"
        , usepackage ["T1"] "fontenc"
        , usepackage ["utf8"] "inputenc"
        , usepackage [] "fancyhdr"
        , usepackage [] "lastpage"
        , Raw "\\pagestyle{fancy}"
        , Raw "\\fancyhf{}"
        ]

getIdentifier :: IdentifierFormat -> Int -> LT.Text
getIdentifier (FormatString []) _ = mempty
getIdentifier (FormatString (StringAtom s : rest)) i =
    LT.pack s <> getIdentifier (FormatString rest) i
getIdentifier (FormatString (PlaceholderAtom a : rest)) i =
    case a of
        Arabic -> LT.pack (show i) <> getIdentifier (FormatString rest) i
        AlphabeticLower -> LT.pack [chr ((i - 1) `mod` 27 + 97)] <> getIdentifier (FormatString rest) i
        AlphabeticUpper -> LT.pack [chr ((i - 1) `mod` 27 + 65)] <> getIdentifier (FormatString rest) i

getEnumStyle :: IdentifierFormat -> KeyFormat -> LT.Text
getEnumStyle ident key = "label=" <> buildKey (getEnumIdentifier' ident) key
  where
    buildKey :: LT.Text -> KeyFormat -> LT.Text
    buildKey _ ((FormatString [])) = mempty
    buildKey i ((FormatString (StringAtom s : rest))) =
        LT.pack s <> buildKey i (FormatString rest)
    buildKey
        i
        ((FormatString (PlaceholderAtom KeyIdentifierPlaceholder : rest))) =
            i <> buildKey i (FormatString rest)

    getEnumIdentifier' :: IdentifierFormat -> LT.Text
    getEnumIdentifier' (FormatString []) = mempty
    getEnumIdentifier' (FormatString (StringAtom s : rest)) =
        LT.pack s <> getEnumIdentifier' (FormatString rest)
    getEnumIdentifier' (FormatString (PlaceholderAtom a : rest)) =
        case a of
            Arabic -> "\\arabic*" <> getEnumIdentifier' (FormatString rest)
            AlphabeticLower -> "\\alph*" <> getEnumIdentifier' (FormatString rest)
            AlphabeticUpper -> "\\Alph*" <> getEnumIdentifier' (FormatString rest)

formatHeaderFooterItem
    :: LaTeX -> LaTeX -> LaTeX -> HeaderFooterItemFormat -> LaTeX
formatHeaderFooterItem superTitle title date (HeaderFooterItemFormat fontsize styles fstring) =
    applyTextStyle fontsize $
        foldr (\style acc -> acc . applyTextStyle style) id styles $
            formatHeaderFooterFstring fstring
  where
    formatHeaderFooterFstring :: FormatString HeaderFooterFormatAtom -> LaTeX
    formatHeaderFooterFstring (FormatString []) = mempty
    formatHeaderFooterFstring (FormatString (StringAtom s : rest)) =
        Sequence (map replace s) <> formatHeaderFooterFstring (FormatString rest)
      where
        replace '\n' = linebreak
        replace c = Text (LT.pack [c])
    formatHeaderFooterFstring (FormatString (PlaceholderAtom a : rest)) =
        case a of
            HeaderFooterSuperTitleAtom -> superTitle <> formatHeaderFooterFstring (FormatString rest)
            HeaderFooterTitleAtom -> title <> formatHeaderFooterFstring (FormatString rest)
            HeaderFooterDateAtom -> date <> formatHeaderFooterFstring (FormatString rest)
            HeaderFooterCurPageNumAtom -> Raw "\\thepage" <> formatHeaderFooterFstring (FormatString rest)
            HeaderFooterLastPageNumAtom -> Raw "\\pageref{LastPage}" <> formatHeaderFooterFstring (FormatString rest)
