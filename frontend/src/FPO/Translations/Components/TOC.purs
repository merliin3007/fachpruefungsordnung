module FPO.Translations.Components.TOC where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type TocLabels =
  ( "toc_end_dropzone"
      ::: SNil
  )

enTOC :: Translation TocLabels
enTOC = fromRecord
  { toc_end_dropzone: "Drop here to add to end of section"
  }

deTOC :: Translation TocLabels
deTOC = fromRecord
  { toc_end_dropzone: "Am Ende einfügen"
  }
