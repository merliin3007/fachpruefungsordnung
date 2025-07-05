module FPO.Translations.Page.Admin.GroupProjects where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type GroupProjectsPageLabels =
  ( "gp_groupProjects"
      ::: "gp_projectManagement"
      ::: SNil
  )

enGroupProjectsPage :: Translation GroupProjectsPageLabels
enGroupProjectsPage = fromRecord
  { gp_groupProjects: "Projects of Group"
  , gp_projectManagement: "Project Management"
  }

deGroupProjectsPage :: Translation GroupProjectsPageLabels
deGroupProjectsPage = fromRecord
  { gp_groupProjects: "Projekte der Gruppe"
  , gp_projectManagement: "Projektverwaltung"
  }