{-# LANGUAGE OverloadedStrings #-}

module Versioning.Hash
  ( Hashable (..),
    Hashed (..),
    Hash (..),
    hashed,
    hashToBS,
  )
where

import qualified Crypto.Hash.SHA1       as SHA1
import           Data.Aeson             (ToJSON (..), (.=))
import qualified Data.Aeson             as Aeson
import           Data.ByteString        (ByteString)
import           Data.ByteString.Base64 (encode)
import           Data.ByteString.Char8  (pack)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE
import           GHC.Int

newtype Hash = Hash ByteString deriving (Show)

hashToBS :: Hash -> ByteString
hashToBS (Hash bs) = bs

instance ToJSON Hash where
  toJSON (Hash bs) = Aeson.String $ TE.decodeUtf8 $ encode bs

class Hashable a where
  updateHash :: SHA1.Ctx -> a -> SHA1.Ctx

  hash :: a -> Hash
  hash = Hash . SHA1.finalize . updateHash SHA1.init

instance Hashable ByteString where
  updateHash = SHA1.update
  hash = Hash . SHA1.hash

instance Hashable Hash where
  updateHash ctx (Hash bs) = updateHash ctx bs
  hash (Hash bs) = hash bs

instance Hashable Int where
  updateHash = updateHashShow

instance Hashable Int32 where
  updateHash = updateHashShow

instance (Hashable a) => Hashable [a] where
  updateHash = foldr $ flip updateHash

instance (Hashable a) => Hashable (Maybe a) where
  updateHash ctx (Just a) = updateHash ctx a
  updateHash ctx _        = ctx

instance Hashable Text where
  updateHash ctx text = updateHash ctx $ TE.encodeUtf8 text

data Hashed a = Hashed Hash a deriving (Show)

instance (Hashable a, ToJSON a) => ToJSON (Hashed a) where
  toJSON (Hashed h content) =
    Aeson.object ["hash" .= h, "content" .= content]

hashed :: (Hashable a) => a -> Hashed a
hashed x = Hashed (hash x) x

updateHashShow :: (Show a) => SHA1.Ctx -> a -> SHA1.Ctx
updateHashShow ctx = SHA1.update ctx . pack . show
