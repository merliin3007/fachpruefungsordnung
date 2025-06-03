module Translations.Translator where

import Prelude

import Effect (Effect)
import Simple.I18n.Translator (Translator, createTranslator)
import Translations.Labels (Labels, de, en)
import Type.Proxy (Proxy(Proxy))
import Web.HTML (window)
import Web.HTML.Navigator (language)
import Web.HTML.Window (navigator)

translator :: EqTranslator
translator =
  EqTranslator $ createTranslator
    (Proxy :: _ "en") -- Fallback language (and default language)
    { en, de } -- Translations

newtype EqTranslator = EqTranslator (Translator Labels)

instance eqEqTranslator :: Eq EqTranslator where
  eq _ _ = false

fromEqTranslator :: EqTranslator -> Translator Labels
fromEqTranslator (EqTranslator trans) = trans

detectBrowserLanguage :: Effect String
detectBrowserLanguage = do
  nav <- window >>= navigator
  maybeLang <- language nav
  pure $ maybeLang

getTranslatorForLanguage :: String -> Translator Labels
getTranslatorForLanguage lang = case lang of
  "de-DE" -> createTranslator (Proxy :: _ "de") { en, de }
  _ -> createTranslator (Proxy :: _ "en") { en, de }
