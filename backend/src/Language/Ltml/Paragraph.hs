module Language.Ltml.Paragraph where

import Data.Text (Text)
import Language.Ltml.Format (IdentifierFormat)
import Language.Ltml.Label (Label)

data Paragraph
  = Paragraph
      ParagraphFormat
      [RichTextTree]

newtype ParagraphFormat
  = ParagraphFormat
      IdentifierFormat

data RichTextTree
  = TextLeaf Text
  | SentenceStart (Maybe Label)
  | Reference Text
  | Styled FontStyle [RichTextTree]
  | EnumItem [RichTextTree]
  | Footnote Text
  deriving (Show)

data FontStyle
  = Bold
  | Italics
  | Underlined
  deriving (Show)
