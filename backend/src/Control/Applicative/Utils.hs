{-# LANGUAGE FlexibleInstances #-}

module Control.Applicative.Utils
    ( (<:>)
    )
where

instance (Applicative f, Semigroup a) => Semigroup (f a) where
    f <> g = (<>) <$> f <*> g

infixl 4 <:>

(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
p <:> q = (:) <$> p <*> q
