module Language.Ltml.HTML.FormatString (sectionFormat, headingFormat, paragraphFormat) where

import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (..))
import Language.Lsd.AST.Type.Section (SectionFormat (..))
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
