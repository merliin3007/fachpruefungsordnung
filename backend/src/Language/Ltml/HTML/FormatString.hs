{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.FormatString (sectionFormat, headingFormat, paragraphFormat, enumFormat, buildEnumCounter) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State, gets, modify)
import Data.Text (Text, pack)
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Enum (EnumFormat (..), EnumItemFormat (..))
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (..))
import Language.Lsd.AST.Type.Section (SectionFormat (..))
import Language.Ltml.HTML.CSS.CustomClay
    ( Counter
    , counterChar
    , counterCharCapital
    , counterNum
    , stringCounter
    )
import Language.Ltml.HTML.Common
    ( GlobalState (enumStyles, mangledEnumCounterID, mangledEnumCounterName)
    , ReaderState
    )
import Language.Ltml.HTML.Util
import Lucid (Html, ToHtml (toHtml))
import Prelude hiding (id)

-- | Builds Heading text based on given FormatString, id and text
headingFormat :: HeadingFormat -> Html () -> Html () -> Html ()
headingFormat (FormatString []) _ _ = mempty
headingFormat (FormatString (a : as)) id text =
    case a of
        -- \| replaces '\n' with <br>
        StringAtom s -> convertNewLine s
        PlaceholderAtom IdentifierPlaceholder -> id
        PlaceholderAtom HeadingTextPlaceholder -> toHtml text
        <> headingFormat (FormatString as) id text

-- | Returns (ID Html, ToC Key Html) for a Section;
--   uses ID Html to build ToC Key Html
sectionFormat :: SectionFormat -> Int -> (Html (), Html ())
sectionFormat (SectionFormat idFormat (TocKeyFormat tocKeyFormat)) = idKeyFormat idFormat tocKeyFormat

-- | Returns (ID Html, Key Html) for a Paragraph;
--   uses ID Html to build Paragraph Key Html
paragraphFormat :: ParagraphFormat -> Int -> (Html (), Html ())
paragraphFormat (ParagraphFormat idFormat (ParagraphKeyFormat paragraphKeyFormat)) = idKeyFormat idFormat paragraphKeyFormat

-- | Builds key html based on identifier html and returns both
idKeyFormat :: IdentifierFormat -> KeyFormat -> Int -> (Html (), Html ())
idKeyFormat idFormat keyFormatS i =
    let idHtml = identifierFormat idFormat i
        keyHtml = keyFormat keyFormatS idHtml
     in (idHtml, keyHtml)

-------------------------------------------------------------------------------

-- | Builds id Html based on given FormatString and id.
identifierFormat
    :: IdentifierFormat -> Int -> Html ()
identifierFormat (FormatString []) _ = return mempty
identifierFormat (FormatString (a : as)) id =
    let b = case a of
            -- \| replaces '\n' with <br>
            StringAtom s -> convertNewLine s
            PlaceholderAtom Arabic -> toHtml $ show id
            -- \| convert paragraphID to single letter string
            PlaceholderAtom AlphabeticLower -> toHtml $ intToLower id
            PlaceholderAtom AlphabeticUpper -> toHtml $ intToCapital id
        bs = identifierFormat (FormatString as) id
     in b <> bs

-- | Builds the desired key in Html based on the given FormatString and the identifier Html
keyFormat :: KeyFormat -> Html () -> Html ()
keyFormat (FormatString []) _ = mempty
keyFormat (FormatString (a : as)) idHtml =
    let b = case a of
            StringAtom s -> toHtml s
            PlaceholderAtom KeyIdentifierPlaceholder -> idHtml
        bs = keyFormat (FormatString as) idHtml
     in b <> bs

-------------------------------------------------------------------------------

-- | Converts EnumFormat to CSS Counter property
enumFormat
    :: EnumFormat -> ReaderT ReaderState (State GlobalState) Text
enumFormat enumFormatS =
    do
        globalEnumStyles <- gets enumStyles
        let mId = lookup enumFormatS globalEnumStyles
         in case mId of
                Just htmlId -> return htmlId
                -- \| Build new mangled css class name
                Nothing -> do
                    mangledEnumName <- gets mangledEnumCounterName
                    mangledEnumId <- gets mangledEnumCounterID
                    let mangledClassName = mangledEnumName <> pack (show mangledEnumId)
                     in do
                            -- \| Add new enumStyle to global map
                            modify
                                (\s -> s {enumStyles = (enumFormatS, mangledClassName) : enumStyles s})
                            -- \| Increment ID for next mangled name
                            modify (\s -> s {mangledEnumCounterID = mangledEnumCounterID s + 1})
                            return mangledClassName

buildEnumCounter :: EnumFormat -> Counter
buildEnumCounter (EnumFormat (EnumItemFormat idFormat (EnumItemKeyFormat enumKeyFormat))) =
    let idCounter = idFormatCounter idFormat
        keyCounter = keyFormatCounter enumKeyFormat idCounter
     in keyCounter

-- | Converts IdentifierFormat to CSS counter
idFormatCounter :: IdentifierFormat -> Counter
idFormatCounter (FormatString []) = mempty
idFormatCounter (FormatString (a : as)) =
    let c = case a of
            StringAtom s -> stringCounter $ pack s
            PlaceholderAtom Arabic -> counterNum "item"
            PlaceholderAtom AlphabeticLower -> counterChar "item"
            PlaceholderAtom AlphabeticUpper -> counterCharCapital "item"
        cs = idFormatCounter (FormatString as)
     in c <> cs

-- | Converts KeyFormat and given identifier Counter to CSS Counter
keyFormatCounter :: KeyFormat -> Counter -> Counter
keyFormatCounter (FormatString []) _ = mempty
keyFormatCounter (FormatString (a : as)) idCounter =
    let c = case a of
            StringAtom s -> stringCounter $ pack s
            PlaceholderAtom KeyIdentifierPlaceholder -> idCounter
        cs = keyFormatCounter (FormatString as) idCounter
     in c <> cs
