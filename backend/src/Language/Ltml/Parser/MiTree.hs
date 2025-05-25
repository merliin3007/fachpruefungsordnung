{-# LANGUAGE ScopedTypeVariables #-}

{- | Parsing Mixed Indentation Trees---trees that may both be represented by
  indentation, or by paranthesis tokens, where the latter nodes may span
  multiple lines of matching indentation.
-}
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
    ( Parser
    , checkIndent
    , eoi
    , nextIndentLevel
    , nli
    )
import Text.Megaparsec (Pos, takeWhile1P, takeWhileP)
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

sp :: Parser Text
sp = takeWhileP (Just "spaces") (== ' ')

sp1 :: Parser Text
sp1 = takeWhile1P (Just "spaces") (== ' ')

sp' :: (FromWhitespace a) => Parser a
sp' = fromWhitespace <$> sp

nli' :: (FromWhitespace a) => Parser a
nli' = fromWhitespace <$> nli

{- | Parse a list of mixed indentation trees (a forest).
  In-line nodes are parsed by @'elementPF' p@, where @p@ is supplied as the
  parser for the children nodes.
  Indented nodes are parsed by 'childP'.
  'elementPF' is expected to be a simple wrapper around its argument @p@,
  and not to consume (ASCII) spaces or newlines, unlike @p@.
  For leaf nodes, 'elementPF' may simply ignore @p@.
  'elementPF p' is further expected to not accept the empty input.
  Unlike 'elementPF', 'childP' is expected to take care of indentation
  itself--past its first line--and to consume any trailing (ASCII) spaces
  or (single) newline plus indentation.
  Typically, 'childP' uses 'hangingBlock'.
-}
miForest
    :: forall a
     . (FromWhitespace a)
    => (Parser [a] -> Parser a)
    -> Parser a
    -> Parser [a]
miForest elementPF childP = L.indentLevel >>= goFrom
  where
    -- Each of the below parsers must generally consume any trailing ASCII
    -- spaces, or newline plus subsequent indentation.

    goFrom :: Pos -> Parser [a]
    goFrom lvl = goE
      where
        -- Parse forest, headed by element.
        goE :: Parser [a]
        goE =
            elementPF goE
                <:> ( (nli' >>= goEorCorEnd')
                        <|> sp' <:> goE
                        <|> pure []
                    )

        -- Parse forest, headed by child; at start of an input line.
        -- Enforces that a child ends after newline ('eoi').
        goC :: Parser [a]
        goC = (checkIndent lvl' *> childP <* eoi lvl') <:> goEorCorEnd
          where
            lvl' = nextIndentLevel lvl

        -- Parse forest, headed by element or child, or end (of forest); at
        -- start of an input line.
        goEorCorEnd :: Parser [a]
        goEorCorEnd = checkIndent lvl *> goE <|> goC <|> pure []

        -- Parse forest, headed by element or child, or end (of forest); at
        -- start of an input line.
        -- In case of element, prepends preceding whitespace encoding
        -- (separator).
        goEorCorEnd' :: a -> Parser [a]
        goEorCorEnd' sep =
            ((sep :) <$> (checkIndent lvl *> goE)) <|> goC <|> pure []

hangingBlock
    :: (FromWhitespace a)
    => Parser ()
    -> (Parser [a] -> Parser a)
    -> Parser a
    -> Parser [a]
hangingBlock keywordP elementPF childP = do
    lvl' <- nextIndentLevel <$> L.indentLevel
    keywordP
    void sp1 <|> void nli <* checkIndent lvl'
    miForest elementPF childP <* eoi lvl'

hangingBlock'
    :: (FromWhitespace a)
    => Text
    -> (Parser [a] -> Parser a)
    -> Parser a
    -> Parser [a]
hangingBlock' = hangingBlock . void . string
