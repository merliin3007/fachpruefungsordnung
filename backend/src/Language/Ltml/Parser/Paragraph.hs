module Language.Ltml.Parser.Paragraph
    ( paragraphP
    )
where

import Control.Applicative (optional)
import Control.Monad.State (evalStateT)
import Language.Lsd.AST.Type.Paragraph (ParagraphType (ParagraphType))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.Parser (Parser, nonIndented)
import Language.Ltml.Parser.Common.Lexeme (nLexeme1)
import Language.Ltml.Parser.Label (labelingP)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec (try)

paragraphP :: ParagraphType -> Parser (Node Paragraph)
paragraphP (ParagraphType fmt tt) = flip evalStateT True $ do
    -- TODO: Avoid `try`.
    label <- nonIndented . optional . try $ nLexeme1 labelingP
    Node label . Paragraph fmt <$> nonIndented (textForestP tt)
