module Language.Ltml.Parser.Section
    ( sectionP
    )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bitraversable (bitraverse)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.Section
    ( HeadingType (HeadingType)
    , SectionType (SectionType)
    )
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    )
import Language.Ltml.Parser (Parser, nonIndented)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Paragraph (paragraphP)
import Language.Ltml.Parser.Text (hangingTextP')
import Text.Megaparsec (many)

sectionP :: SectionType -> Parser () -> Parser (Node Section)
sectionP (SectionType kw headingT fmt childrenT) succStartP = do
    (mLabel, heading) <- nonIndented $ headingP kw headingT
    Node mLabel . Section fmt heading
        <$> nonIndented (bitraverse parsP secsP childrenT)
  where
    parsP :: ParagraphType -> Parser [Node Paragraph]
    parsP t = many $ nLexeme $ paragraphP t succStartP

    secsP :: SectionType -> Parser [Node Section]
    secsP t = many $ sectionP t (toStartP t <|> succStartP)

toStartP :: SectionType -> Parser ()
toStartP (SectionType kw _ _ _) = void $ keywordP kw

headingP :: Keyword -> HeadingType -> Parser (Maybe Label, Heading)
headingP kw (HeadingType fmt tt) =
    nLexeme $ fmap (Heading fmt) <$> hangingTextP' kw tt
