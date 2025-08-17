{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Common.Lexeme
    ( sp
    , sp1
    , nLexeme
    , nLexeme1
    )
where

import Control.Monad (void)
import Data.Text (Text)
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)

sp :: (MonadParser m) => m Text
sp = takeWhileP (Just "spaces") (== ' ')

sp1 :: (MonadParser m) => m Text
sp1 = takeWhile1P (Just "spaces") (== ' ')

-- | Lexeme parser combinator that permits newlines and ASCII spaces.
nLexeme :: (MonadParser m) => m a -> m a
nLexeme = L.lexeme nSc

-- | Like 'nLexeme', but require at least one newline.
--   Useful for parsers that do not themselves require a final newline.
nLexeme1 :: (MonadParser m) => m a -> m a
nLexeme1 p = nLexeme (p <* sp <* char '\n')

nSc :: (MonadParser m) => m ()
nSc = void $ takeWhileP (Just "whitespace") (\c -> c == '\n' || c == ' ')
