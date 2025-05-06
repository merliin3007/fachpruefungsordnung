module Database (getConnection, getPool) where

import Data.ByteString.Char8 (pack)
import Data.Functor
import qualified Hasql.Connection as Conn
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import System.Environment

getPool :: IO Pool.Pool
getPool = do
  s <- envSettings
  Pool.acquire $ PoolConfig.settings [PoolConfig.staticConnectionSettings s]

getConnection :: IO (Either Conn.ConnectionError Conn.Connection)
getConnection = envSettings >>= Conn.acquire

envSettings :: IO Conn.Settings
envSettings = do
  host <- getEnv "POSTGRES_HOST" <&> pack
  port <- getEnv "POSTGRES_PORT" <&> read
  user <- getEnv "POSTGRES_USER" <&> pack
  password <- getEnv "POSTGRES_PASSWORD" <&> pack
  database <- getEnv "POSTGRES_DB" <&> pack
  return $ Conn.settings host port user password database
