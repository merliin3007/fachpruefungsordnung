module Translations.Translator where

import Prelude

import Halogen.Store.Monad (getStore)
import Simple.I18n.Translator (Translator, createTranslator, translate)
import Translations.Labels (Labels, de, en)
import Type.Proxy (Proxy(Proxy))

translator :: EqTranslator
translator =
  EqTranslator $ createTranslator
    (Proxy :: _ "de") -- Fallback language (and default language)
    { en, de } -- Translations

newtype EqTranslator = EqTranslator (Translator Labels)

instance eqEqTranslator :: Eq EqTranslator where
  eq _ _ = true

fromEqTranslator :: EqTranslator -> Translator Labels
fromEqTranslator (EqTranslator translator) = translator
