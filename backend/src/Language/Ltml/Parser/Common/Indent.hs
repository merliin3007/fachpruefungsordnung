{-# LANGUAGE FlexibleContexts #-}

-- | Parsers and parser combinators for handling indented text.
--
--   They generally expect to be run at the start of an input line, after any
--   indentation (ASCII spaces; usually after 'nli').
--   (Compare how typical lexeme parser combinators are expected to be run
--   after any whitespace.)
module Language.Ltml.Parser.Common.Indent
    ( nli
    , nextIndentLevel
    , nonIndented
    , someIndented
    , checkIndent
    , eoi
    )
where

import Language.Ltml.Parser (MonadParser)
import Language.Ltml.Parser.Common.Lexeme (sp)

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text (cons)
import Text.Megaparsec
    ( Pos
    , eof
    , mkPos
    , sepBy1
    , takeWhileP
    , (<?>)
    )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
    ( incorrectIndent
    , indentLevel
    )
import Text.Megaparsec.Pos (pos1)

nextIndentLevel :: Pos -> Pos
nextIndentLevel = (<> mkPos 2)

-- | Parse a newline character and any subsequent indentation (ASCII spaces).
nli :: (MonadParser m) => m Text
nli =
    Text.cons
        <$> char '\n'
        <*> takeWhileP (Just "indentation") (== ' ')

-- | Given a parser, adapt it to parse non-indented data (only), where the
--   empty string counts as non-indented at EOF.
--   The latter is to allow for sequencing so-adapted parsers that terminate
--   on each of newline and EOF, where either a sequenced parser accepts the
--   empty input, or the sequencing is done via 'Text.Megaparsec.many' or
--   similar.
nonIndented :: (MonadParser m) => m a -> m a
nonIndented p = (eof <|> checkIndent pos1) *> p

-- | Parse some (>= 1) indented items, all with the same indentation level.
--   The argument parser is expected to consume any trailing indentation.
someIndented :: (MonadParser m) => m a -> m [a]
someIndented p = L.indentLevel >>= (\lvl -> p `sepBy1` checkIndent lvl)

-- | Check whether the current actual indentation matches the current required
--   indentation level.
checkIndent :: (MonadParser m) => Pos -> m ()
checkIndent lvl = do
    pos <- L.indentLevel
    guard (pos == lvl) <|> L.incorrectIndent EQ lvl pos

-- | Check for End Of Indentation scope (whether actual indentation is less
--   then current indentation level, or eof is reached).
eoi :: (MonadParser m) => Pos -> m ()
eoi lvl = (decrIndent <|> eof) <?> "end of indentation scope"
  where
    decrIndent = L.indentLevel >>= guard . (< lvl)
