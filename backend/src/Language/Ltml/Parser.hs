{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser
    ( Parser
    , nli
    , nextIndentLevel
    , checkIndent
    , eoi
    )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text (cons)
import Data.Void (Void)
import Text.Megaparsec
    ( MonadParsec
    , Parsec
    , Pos
    , eof
    , mkPos
    , takeWhileP
    , (<?>)
    )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

type Parser = Parsec Void Text

nextIndentLevel :: Pos -> Pos
nextIndentLevel = (<> mkPos 2)

-- | Parse a newline character and any subsequent indentation (ASCII spaces).
nli :: (MonadParsec e Text m) => m Text
nli =
    Text.cons
        <$> char '\n'
        <*> takeWhileP (Just "indentation") (== ' ')

-- | Check whether the current actual indentation matches the current required
--   indentation level.
--   This parser is expected to be run at the start of an input line, after
--   any indentation (ASCII spaces; usually after 'nli').
checkIndent :: (MonadParsec e Text m, MonadFail m) => Pos -> m ()
checkIndent lvl = do
    pos <- L.indentLevel
    guard (pos == lvl) <|> fail "Incorrect indentation."

-- | Check for End Of Indentation scope (whether actual indentation is less
--   then current indentation level, or eof is reached).
eoi :: (MonadParsec e Text m) => Pos -> m ()
eoi lvl = (decrIndent <|> eof) <?> "end of indentation scope"
  where
    decrIndent = L.indentLevel >>= guard . (< lvl)
