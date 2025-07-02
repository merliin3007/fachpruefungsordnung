{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Common.Lexeme
    ( nLexeme
    )
where

import Control.Monad (void)
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (takeWhileP)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)

-- | Lexeme parser combinator that permits newlines and ASCII spaces.
nLexeme :: (MonadParser m) => m a -> m a
nLexeme = L.lexeme nSc

nSc :: (MonadParser m) => m ()
nSc = void $ takeWhileP (Just "whitespace") (\c -> c == '\n' || c == ' ')
