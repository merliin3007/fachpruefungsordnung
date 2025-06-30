{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToPandoc.Text
    ( textBlockW
    )
where

import Control.Monad.Reader (asks)
import Data.Either.Utils (leftMerge, ltrMerge)
import Data.List (singleton)
import qualified Data.Map as Map (lookup)
import Data.Text (Text)
import Data.Void (Void, absurd)
import Language.Ltml.AST.Label (unLabel)
import Language.Ltml.AST.Text
    ( EnumItem (EnumItem)
    , FontStyle (..)
    , SentenceStart (SentenceStart)
    , TextTree (..)
    )
import Language.Ltml.ToPandoc (ToPandoc)
import qualified Text.Pandoc.Definition as P
    ( Block (OrderedList, Plain)
    , Inline (..)
    , ListNumberDelim (DefaultDelim)
    , ListNumberStyle (DefaultStyle)
    , Target
    )

textBlockW
    :: (StyleW style, EnumW enumItem, SpecialW special)
    => [TextTree style enumItem special]
    -> ToPandoc [P.Block]
textBlockW xs = ltrMerge P.Plain . concat <$> mapM textTreeW xs

textTreeW
    :: (StyleW style, EnumW enumItem, SpecialW special)
    => TextTree style enumItem special
    -> ToPandoc [Either P.Inline P.Block]
textTreeW (Word w) = rsi $ P.Str w
textTreeW Space = rsi P.Space
textTreeW (Special special) = rsi $ specialW special
textTreeW (Reference lbl) = do
    (ident, tgt) <- asks (f . Map.lookup lbl)
    rsi $ P.Link ("", [], []) [P.Str ident] tgt
  where
    f :: Maybe Text -> (Text, P.Target)
    f Nothing = ("??", ("", "Broken reference"))
    f (Just ident) = (ident, ("#" <> unLabel lbl, "" {- TODO -}))
textTreeW (Styled style xs) =
    leftMerge (styled style) . concat <$> mapM textTreeW xs
-- TODO: Merge enum children (best elsewhere).
textTreeW (EnumChild enumItem) = sb <$> enumW enumItem
textTreeW (Footnote xs) = si . P.Note <$> textBlockW xs

rsi :: P.Inline -> ToPandoc [Either P.Inline P.Block]
rsi = return . si

si :: P.Inline -> [Either P.Inline P.Block]
si = singleton . Left

sb :: P.Block -> [Either P.Inline P.Block]
sb = singleton . Right

class SpecialW special where
    specialW :: special -> P.Inline

instance SpecialW Void where
    specialW = absurd

instance SpecialW SentenceStart where
    specialW (SentenceStart Nothing) = P.Span ("", [], []) []
    specialW (SentenceStart (Just lbl)) = P.Span (unLabel lbl, [], []) []

class StyleW style where
    styled :: style -> [P.Inline] -> P.Inline

instance StyleW Void where
    styled = absurd

instance StyleW FontStyle where
    styled Bold = P.Strong
    styled Italics = P.Emph
    styled Underlined = P.Underline

class EnumW enumItem where
    enumW :: enumItem -> ToPandoc P.Block

instance EnumW Void where
    enumW = absurd

instance EnumW EnumItem where
    enumW (EnumItem xs) =
        P.OrderedList (1, P.DefaultStyle, P.DefaultDelim)
            . singleton
            <$> textBlockW xs
