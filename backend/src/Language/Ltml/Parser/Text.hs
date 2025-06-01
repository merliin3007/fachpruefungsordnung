{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Text
    ( TextParser
    , TextContext (..)
    , textForestP
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Combinators (choice)
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.Char as Char (isAlpha)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Ltml.AST.Text
    ( FontStyle (..)
    , TextTree (..)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Label (labelP)
import Language.Ltml.Parser.MiTree (hangingBlock', miForest)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (char)

data TextContext = TextContext
    { textEnumTypes :: [EnumType]
    }

type TextParser = ReaderT TextContext Parser

textForestP :: (RichP r, EnumP e, ParagraphP p) => TextParser [TextTree r e p]
textForestP = miForest elementPF childP
  where
    elementPF
        :: (RichP r, ParagraphP p)
        => TextParser [TextTree r e p]
        -> TextParser (TextTree r e p)
    elementPF p =
        textLeafP
            <|> sentenceStartP
            <|> Reference <$ char '{' <*> labelP <* char '}'
            <|> styledP p

    childP :: (RichP r, EnumP e) => TextParser (TextTree r e p)
    childP =
        enumItemP
            <|> Footnote <$> hangingBlock' "^" elementPF childP

class RichP (r :: Bool) where
    styledP :: TextParser [TextTree r e p] -> TextParser (TextTree r e p)

instance RichP 'False where
    styledP = const empty

instance RichP 'True where
    styledP p = Styled <$ char '<' <*> fontStyleP <*> p <* char '>'

fontStyleP :: TextParser FontStyle
fontStyleP =
    Bold <$ char '*'
        <|> Italics <$ char '/'
        <|> Underlined <$ char '_'

class EnumP (e :: Bool) where
    enumItemP :: TextParser (TextTree r e p)

instance EnumP 'True where
    enumItemP = asks textEnumTypes >>= choice . fmap enumItemP'
      where
        enumItemP' :: EnumType -> TextParser (TextTree r 'True p)
        -- TODO
        enumItemP' _ = EnumItem <$> hangingBlock' "#" undefined undefined

instance EnumP 'False where
    enumItemP = empty

class ParagraphP (p :: Bool) where
    textLeafP :: TextParser (TextTree r e p)
    sentenceStartP :: TextParser (TextTree r e p)

instance ParagraphP 'True where
    textLeafP = TextLeaf <$> takeWhile1P (Just "word") Char.isAlpha -- TODO
    sentenceStartP = SentenceStart <$> empty -- TODO

instance ParagraphP 'False where
    textLeafP = TextLeaf <$> takeWhile1P (Just "word") Char.isAlpha -- TODO
    sentenceStartP = empty
