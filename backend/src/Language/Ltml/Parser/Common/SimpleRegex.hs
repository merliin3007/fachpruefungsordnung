{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Common.SimpleRegex
    ( simpleRegexP
    )
where

import Control.Applicative.Combinators (choice)
import Language.Lsd.AST.SimpleRegex
    ( Disjunction (Disjunction)
    , Sequence (Sequence)
    , SimpleRegex (SimpleRegex)
    , Star (Star)
    )
import Language.Lsd.AST.SimpleRegex.Utils (Heads, mapWithSuccs)
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (many)

-- | Parse input according to a 'SimpleRegex', taking into account not only
--   the respective types, but also the respective possible successors.
simpleRegexP
    :: (MonadParser m)
    => (t -> Heads t -> m a)
    -> SimpleRegex t
    -> m [a]
simpleRegexP f sre =
    concat
        <$> sequence
            [ sequenceP id prefix
            , middleP id middle
            , sequenceP id suffix
            ]
  where
    SimpleRegex prefix middle suffix = mapWithSuccs f sre

starP :: (MonadParser m) => (a -> m b) -> Star a -> m [b]
starP f (Star x) = many $ f x

disjunctionP :: (MonadParser m) => (a -> m b) -> Disjunction a -> m b
disjunctionP f (Disjunction xs) = choice (fmap f xs)

sequenceP :: (MonadParser m) => (a -> m b) -> Sequence a -> m [b]
sequenceP f (Sequence xs) = mapM f xs

middleP
    :: (MonadParser m)
    => (a -> m b)
    -> Disjunction (Star (Disjunction a))
    -> m [b]
middleP = disjunctionP . starP . disjunctionP
