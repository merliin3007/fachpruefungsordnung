module Data.Text.FromWhitespace
  ( FromWhitespace,
    fromWhitespace,
  )
where

import           Data.Text (Text)

class FromWhitespace a where
  fromWhitespace :: Text -> a
