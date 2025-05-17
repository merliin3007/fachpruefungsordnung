module Versioning.Error (VersionControlError (..)) where

data VersionControlError
    = InsufficientPrevilige
    | DatabaseError String
    deriving (Eq, Show)
