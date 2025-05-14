module Language.Ltml.Parser
  ( Parser,
    parseTest,
    nli,
    indent,
    checkIndent,
    eoi,
  )
where

import           Control.Applicative        ((<|>))
import           Control.Applicative.Aux    ()
import           Control.Monad              (guard)
import           Control.Monad.State.Strict (StateT, get, modify, runStateT)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (singleton)
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as MP (parseTest)
import           Text.Megaparsec            (Parsec, Pos, eof, mkPos,
                                             takeWhileP, (<?>))
import           Text.Megaparsec.Char       (char)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

newtype PState = PState
  { stIndentLevel :: Pos
  }
  deriving (Show)

type Parser = StateT PState (Parsec Void Text)

initState :: PState
initState = PState (mkPos 1)

nextIndentLevel :: Pos -> Pos
nextIndentLevel = (<> mkPos 2)

parseTest :: (Show a) => Parser a -> Text -> IO ()
parseTest p = MP.parseTest (runStateT p initState)

getIndentLevel :: Parser Pos
getIndentLevel = stIndentLevel <$> get

putIndentLevel :: Pos -> Parser ()
putIndentLevel lvl = modify (\s -> s {stIndentLevel = lvl})

-- | Parse a newline character and any subsequent indentation (ASCII spaces).
nli :: Parser Text
nli =
  (Text.singleton <$> char '\n')
    <> takeWhileP (Just "indentation") (== ' ')

-- | Check whether the current actual indentation matches the current required
--   indentation level.
--   This parser is expected to be run at the start of an input line, after
--   any indentation (ASCII spaces; usually after 'nli').
checkIndent :: Parser ()
checkIndent = do
  lvl <- getIndentLevel
  pos <- L.indentLevel
  guard (pos == lvl) <|> fail "Incorrect indentation."

-- | Check for End Of Indentation scope (whether actual indentation is less
--   then current indentation level, or eof is reached).
eoi :: Parser ()
eoi = (decrIndent <|> eof) <?> "end of indentation scope"
  where
    decrIndent = do
      lvl <- getIndentLevel
      pos <- L.indentLevel
      guard $ pos < lvl

-- | Increase the indentation level for a given parser.
--   This only modifies the indentation state; the actual indentation must be
--   enforced (to match the state) via 'checkIndent').
indent :: Parser a -> Parser a
indent p = do
  lvl <- getIndentLevel
  putIndentLevel $ nextIndentLevel lvl
  x <- p
  putIndentLevel lvl
  return x
