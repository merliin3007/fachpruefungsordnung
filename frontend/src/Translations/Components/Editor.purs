module Translations.Components.Editor where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type EditorLabels =
  ( "editor_fontSizeDown"
      ::: "editor_fontSizeUp"
      ::: "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: SNil
  )

enEditor :: Translation EditorLabels
enEditor = fromRecord
  { editor_fontSizeDown: "Font size up"
  , editor_fontSizeUp: "Font size down"
  , editor_textBold: "Bold text (Ctrl+B)"
  , editor_textItalic: "Italic text (Ctrl+I)"
  , editor_textUnderline: "Underline text"
  }

deEditor :: Translation EditorLabels
deEditor = fromRecord
  { editor_fontSizeDown: "Schrift vergößern"
  , editor_fontSizeUp: "Schrift verkleinern"
  , editor_textBold: "Text fett (Strg+B)"
  , editor_textItalic: "Text kursiv (Strg+I)"
  , editor_textUnderline: "Text unterstreichen"
  }
