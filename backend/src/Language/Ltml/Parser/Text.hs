{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Text
    ( textForestP
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Combinators (choice)
import qualified Data.Char as Char (isAlpha)
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword (..))
import Language.Lsd.AST.Type.Enum (EnumType (..))
import Language.Lsd.AST.Type.Text
    ( FootnoteType (..)
    , TextType (..)
    )
import Language.Ltml.AST.Text
    ( EnumItem (..)
    , FontStyle (..)
    , SentenceStart (..)
    , TextTree (..)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Label (labelP)
import Language.Ltml.Parser.MiTree (hangingBlock', miForest)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (char)

textForestP
    :: (StyleP style, EnumP enumType enumItem, SpecialP special)
    => TextType enumType
    -> Parser [TextTree style enumItem special]
textForestP t = miForest elementPF (childPF t)

elementPF
    :: (StyleP style, SpecialP special)
    => Parser [TextTree style enumItem special]
    -> Parser (TextTree style enumItem special)
elementPF p =
    Special <$> specialP
        <|> textLeafP
        <|> Reference <$ char '{' <*> labelP <* char '}'
        <|> Styled <$ char '<' <*> styleP <*> p <* char '>'

childPF
    :: (StyleP style, EnumP enumType enumItem)
    => TextType enumType
    -> Parser (TextTree style enumItem special)
childPF (TextType enumTypes footnoteTypes) =
    EnumChild <$> choice (fmap enumItemP enumTypes)
        <|> Footnote <$> choice (fmap footnoteP footnoteTypes)

-- TODO: Configurable keyword.
footnoteP
    :: (StyleP style)
    => FootnoteType
    -> Parser [TextTree style Void Void]
footnoteP (FootnoteType tt) = hangingBlock' "^" elementPF (childPF tt)

class StyleP style where
    styleP :: Parser style

instance StyleP Void where
    styleP = empty

instance StyleP FontStyle where
    styleP =
        Bold <$ char '*'
            <|> Italics <$ char '/'
            <|> Underlined <$ char '_'

class EnumP enumType enumItem where
    enumItemP :: enumType -> Parser enumItem

instance EnumP Void Void where
    enumItemP = const empty

instance EnumP EnumType EnumItem where
    enumItemP (EnumType (Keyword kw) tt) =
        EnumItem <$> hangingBlock' kw elementPF (childPF tt)

class SpecialP special where
    specialP :: Parser special
    textLeafP :: Parser (TextTree a b special)

instance SpecialP Void where
    specialP = empty
    textLeafP = TextLeaf <$> takeWhile1P (Just "word") Char.isAlpha -- TODO

instance SpecialP SentenceStart where
    specialP = pure $ SentenceStart Nothing -- TODO
    textLeafP = TextLeaf <$> takeWhile1P (Just "word") Char.isAlpha -- TODO
