{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Lsd.AST.SimpleRegex.Utils
    ( Heads (..)
    , mapWithSuccs
    )
where

import Language.Lsd.AST.SimpleRegex
    ( Disjunction (Disjunction)
    , Sequence (Sequence)
    , SimpleRegex (SimpleRegex)
    , Star (Star)
    )

-- | The possible first tokens of a regex.  The boolean flag signifies whether
--   the regex accepts the empty input (is nullable).
--   Notably also used for the successors of a leaf in a regex.
data Heads a = Heads
    { nHeads :: Bool
    , rHeads :: [a]
    }

unpack :: Heads a -> (Bool, [a])
unpack (Heads b xs) = (b, xs)

-- (<>) corresponds to concatenation of the regexes.
-- I.e., 'heads x <> heads y = heads (x <> y)' (pretending that our regexes
-- can be concatenated via '<>').
instance Semigroup (Heads a) where
    Heads True xs <> Heads b ys = Heads b (xs ++ ys)
    succs@(Heads False _) <> _ = succs

instance Monoid (Heads a) where
    mempty = Heads True []

class GetHeads a fa where
    heads :: fa -> Heads a

class (GetHeads a fa) => MapWithSuccs a b fa fb where
    mapWithSuccs' :: (a -> Heads a -> b) -> Heads a -> fa -> fb

mapWithSuccs
    :: (MapWithSuccs a b fa fb)
    => (a -> Heads a -> b)
    -> fa
    -> fb
mapWithSuccs f = mapWithSuccs' f mempty

instance GetHeads a a where
    heads x = Heads False [x]

instance MapWithSuccs a b a b where
    mapWithSuccs' f succs x = f x succs

instance (GetHeads a fa) => GetHeads a (Star fa) where
    heads (Star x) = Heads True (rHeads $ heads x)

instance
    (MapWithSuccs a b fa fb)
    => MapWithSuccs a b (Star fa) (Star fb)
    where
    mapWithSuccs' f succs (Star x) = Star $ mapWithSuccs' f succs' x
      where
        succs' = Heads True $ rHeads (heads x) ++ rHeads succs

instance (GetHeads a fa) => GetHeads a (Disjunction fa) where
    heads (Disjunction xs) = f $ map heads xs
      where
        f :: [Heads a] -> Heads a
        f zss = Heads (or as) (concat bs)
          where
            (as, bs) = unzip $ fmap unpack zss

instance
    (MapWithSuccs a b fa fb)
    => MapWithSuccs a b (Disjunction fa) (Disjunction fb)
    where
    mapWithSuccs' f succs (Disjunction xs) =
        Disjunction $ map (mapWithSuccs' f succs) xs

instance (GetHeads a fa) => GetHeads a (Sequence fa) where
    heads (Sequence xs) = mconcat $ map heads xs

instance
    (MapWithSuccs a b fa fb)
    => MapWithSuccs a b (Sequence fa) (Sequence fb)
    where
    mapWithSuccs' f succs (Sequence xs) = Sequence (g xs)
      where
        g :: [fa] -> [fb]
        g = snd . foldr h (succs, [])
          where
            h :: fa -> (Heads a, [fb]) -> (Heads a, [fb])
            h x (succs', ys) =
                (heads x <> succs', mapWithSuccs' f succs' x : ys)

instance GetHeads a (SimpleRegex a) where
    heads (SimpleRegex prefix middle suffix) =
        heads prefix <> heads middle <> heads suffix

instance MapWithSuccs a b (SimpleRegex a) (SimpleRegex b) where
    mapWithSuccs' f succs (SimpleRegex prefix middle suffix) =
        SimpleRegex prefix' middle' suffix'
      where
        prefix' = mapWithSuccs' f succsPrefix prefix
        middle' = mapWithSuccs' f succsMiddle middle
        suffix' = mapWithSuccs' f succsSuffix suffix

        succsPrefix = heads middle <> succsMiddle
        succsMiddle = heads suffix <> succsSuffix
        succsSuffix = succs
