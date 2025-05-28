{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing Mixed Indentation Trees---trees that may both be represented by
--   indentation, or by paranthesis tokens, where the latter nodes may span
--   multiple lines of matching indentation.
module Language.Ltml.Parser.MiTree
    ( miForest
    , hangingBlock
    , hangingBlock'
    , sp
    , sp1
    )
where

import Control.Applicative ((<|>))
import Control.Applicative.Utils ((<:>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Language.Ltml.Parser
    ( checkIndent
    , eoi
    , nextIndentLevel
    , nli
    )
import Text.Megaparsec (MonadParsec, Pos, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

sp :: (MonadParsec e Text m) => m Text
sp = takeWhileP (Just "spaces") (== ' ')

sp1 :: (MonadParsec e Text m) => m Text
sp1 = takeWhile1P (Just "spaces") (== ' ')

sp' :: (MonadParsec e Text m, FromWhitespace a) => m a
sp' = fromWhitespace <$> sp

nli' :: (MonadParsec e Text m, FromWhitespace a) => m a
nli' = fromWhitespace <$> nli

-- | Parse a list of mixed indentation trees (a forest).
--   In-line nodes are parsed by @'elementPF' p@, where @p@ is supplied as the
--   parser for the children nodes.
--   Indented nodes are parsed by 'childP'.
--   'elementPF' is expected to be a simple wrapper around its argument @p@,
--   and not to consume (ASCII) spaces or newlines, unlike @p@.
--   For leaf nodes, 'elementPF' may simply ignore @p@.
--   'elementPF p' is further expected to not accept the empty input.
--   Unlike 'elementPF', 'childP' is expected to take care of indentation
--   itself--past its first line--and to consume any trailing (ASCII) spaces
--   or (single) newline plus indentation.
--   Typically, 'childP' uses 'hangingBlock'.
miForest
    :: forall e m a
     . (MonadParsec e Text m, MonadFail m, FromWhitespace a)
    => (m [a] -> m a)
    -> m a
    -> m [a]
miForest elementPF childP = L.indentLevel >>= miForestFrom elementPF childP

miForestFrom
    :: forall e m a
     . (MonadParsec e Text m, MonadFail m, FromWhitespace a)
    => (m [a] -> m a)
    -> m a
    -> Pos
    -> m [a]
miForestFrom elementPF childP lvl = goE
  where
    -- Each of the below parsers must generally consume any trailing ASCII
    -- spaces, or newline plus subsequent indentation.

    -- Parse forest, headed by element.
    goE :: m [a]
    goE =
        elementPF goE
            <:> ( (nli' >>= goEorCorEnd')
                    <|> sp' <:> goE
                    <|> pure []
                )

    -- Parse forest, headed by child; at start of an input line.
    -- Enforces that a child ends after newline ('eoi').
    goC :: m [a]
    goC = (checkIndent lvl' *> childP <* eoi lvl') <:> goEorCorEnd
      where
        lvl' = nextIndentLevel lvl

    -- Parse forest, headed by element or child, or end (of forest); at
    -- start of an input line.
    goEorCorEnd :: m [a]
    goEorCorEnd = checkIndent lvl *> goE <|> goC <|> pure []

    -- Parse forest, headed by element or child, or end (of forest); at
    -- start of an input line.
    -- In case of element, prepends preceding whitespace encoding
    -- (separator).
    goEorCorEnd' :: a -> m [a]
    goEorCorEnd' sep =
        ((sep :) <$> (checkIndent lvl *> goE)) <|> goC <|> pure []

hangingBlock
    :: (MonadParsec e Text m, MonadFail m, FromWhitespace a)
    => m ()
    -> (m [a] -> m a)
    -> m a
    -> m [a]
hangingBlock keywordP elementPF childP = do
    lvl' <- nextIndentLevel <$> L.indentLevel
    keywordP
    void sp1 <|> void nli <* checkIndent lvl'
    miForestFrom elementPF childP lvl' <* eoi lvl'

hangingBlock'
    :: (MonadParsec e Text m, MonadFail m, FromWhitespace a)
    => Text
    -> (m [a] -> m a)
    -> m a
    -> m [a]
hangingBlock' = hangingBlock . void . string
