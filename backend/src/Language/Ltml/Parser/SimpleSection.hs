module Language.Ltml.Parser.SimpleSection
    ( simpleSectionP
    , simpleSectionSequenceP
    )
where

import Control.Applicative.Utils ((<:>))
import Control.Monad (void)
import Language.Lsd.AST.SimpleRegex (Sequence (Sequence))
import Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionType (SimpleSectionType)
    )
import Language.Ltml.AST.SimpleSection (SimpleSection (SimpleSection))
import Language.Ltml.Parser (Parser, sp)
import Language.Ltml.Parser.Common.Combinators (manyTillSucc)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.SimpleParagraph (simpleParagraphP)
import Text.Megaparsec.Char (char)

simpleSectionP :: SimpleSectionType -> Parser () -> Parser SimpleSection
simpleSectionP (SimpleSectionType kw fmt childrenT) succStartP = do
    nLexeme (keywordP kw <* sp <* char '\n')
    SimpleSection fmt
        <$> manyTillSucc (nLexeme $ simpleParagraphP childrenT) succStartP

simpleSectionSequenceP
    :: Sequence SimpleSectionType
    -> Parser ()
    -> Parser (Sequence SimpleSection)
simpleSectionSequenceP (Sequence ts') succStartP = Sequence <$> aux ts'
  where
    aux [] = return []
    aux [t] = simpleSectionP t succStartP <:> aux []
    aux (t : ts@(t' : _)) = simpleSectionP t (toStartP t') <:> aux ts

toStartP :: SimpleSectionType -> Parser ()
toStartP (SimpleSectionType kw _ _) = void $ keywordP kw
