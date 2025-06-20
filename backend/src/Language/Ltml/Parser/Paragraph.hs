module Language.Ltml.Parser.Paragraph
    ( paragraphP
    )
where

import Control.Applicative (optional)
import Language.Lsd.AST.Type.Paragraph (ParagraphType (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Label (labelingP)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char)

-- TODO: Avoid `try`.
paragraphP :: ParagraphType -> Parser (Node Paragraph)
paragraphP (ParagraphType fmt tt) = do
    label <- optional $ try $ labelingP <* char '\n'
    Node label . Paragraph fmt <$> textForestP tt
