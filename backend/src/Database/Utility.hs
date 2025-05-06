module Database.Utility (compressText, hashText) where

import Codec.Compression.Zlib (compress)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

compressText :: Text -> Text
compressText = TE.decodeUtf8 . BS.toStrict . compress . TLE.encodeUtf8 . TL.fromStrict

hashText :: Text -> Text
hashText = TE.decodeUtf8 . hash . TE.encodeUtf8
