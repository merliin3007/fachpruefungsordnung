{-# LANGUAGE OverloadedStrings #-}

module Docs.TestDoc (createTestDocument, createTextElement) where

import Hasql.Connection (Connection)

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.UUID (fromString)

import Docs
import qualified Docs.Document as Document
import Docs.Hasql.Database (run, runTransaction)
import Docs.TextElement (TextElementRef (..))
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( ConflictStatus (..)
    , NewTextRevision (NewTextRevision)
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Edge (..), Node (..), NodeHeader (..), Tree (..))

createTestDocument :: Connection -> IO ()
createTestDocument db = do
    let userID = fromMaybe undefined $ fromString "7f59659a-9a46-4ba0-a911-09698107a6ea"
    let groupID = 1
    doc <- withDB $ run $ createDocument userID groupID "Test Document"
    let docID = Document.identifier doc
    -- paragraphen
    paragraph1 <- withDB $ run $ createTextElement userID docID "paragraph"
    paragraph2 <- withDB $ run $ createTextElement userID docID "paragraph"
    paragraph3 <- withDB $ run $ createTextElement userID docID "paragraph"
    paragraph4 <- withDB $ run $ createTextElement userID docID "paragraph"
    paragraph5 <- withDB $ run $ createTextElement userID docID "paragraph"
    -- anlagen
    anlage1 <- withDB $ run $ createTextElement userID docID "attachement"
    anlage2 <- withDB $ run $ createTextElement userID docID "attachement"
    anlage3 <- withDB $ run $ createTextElement userID docID "attachement"
    anlage4 <- withDB $ run $ createTextElement userID docID "attachement"
    anlage5 <- withDB $ run $ createTextElement userID docID "attachement"
    let tree =
            Node
                (NodeHeader "BodyNode" "DocumentRoot")
                [ Edge "§ 1" (Leaf (TextElement.identifier paragraph1))
                , Edge "§ 2" (Leaf (TextElement.identifier paragraph2))
                , Edge "§ 3" (Leaf (TextElement.identifier paragraph3))
                , Edge "§ 4" (Leaf (TextElement.identifier paragraph4))
                , Edge "§ 5" (Leaf (TextElement.identifier paragraph5))
                , Edge
                    "Anlagen"
                    ( Tree
                        ( Node
                            (NodeHeader "BodyNode" "Attachements")
                            [ Edge "Thomann" (Leaf (TextElement.identifier anlage1))
                            , Edge "LD Systems" (Leaf (TextElement.identifier anlage2))
                            , Edge "Beyerdynamic" (Leaf (TextElement.identifier anlage3))
                            , Edge "Seeburg TSM 15" (Leaf (TextElement.identifier anlage4))
                            , Edge "Audionate" (Leaf (TextElement.identifier anlage5))
                            ]
                        )
                    )
                ]
    _ <- withDB $ runTransaction $ createTreeRevision userID docID tree
    let textElementRef = TextElementRef docID . TextElement.identifier
    let textRevision element = NewTextRevision (textElementRef element)
    let machText = (((withDB . runTransaction . createTextRevision userID) .) .) . textRevision
    eins <- machText paragraph1 Nothing "Das hier ist ein Abschnitt."
    let parent = case eins of
            NoConflict new -> Just $ TextRevision.identifier (TextRevision.header new)
            _ -> Nothing
    _ <-
        machText paragraph2 Nothing $
            pack $
                unlines
                    [ "§{label:} Studienaufbau"
                    , ""
                    , "Das Masterstudium hat eine Regelstudienzeit von vier Semestern."
                    , "Das Studienvolumen umfasst 120 LP und etwa 80 Semesterwochenstunden."
                    , ""
                    , "Es müssen Wahlpflichtmodule im Umfang von insgesamt 90 LP erfolgreich abgeschlossen werden."
                    , "  # Informatikmodule im Gesamtumfang von 75 bis 80 LP aus den folgenden Bereichen:"
                    , "      # Wahlpflichtmodule Informatik (Msc-Inf-WP): Es müssen mindestens 12 LP"
                    , "        aus dem Bereich Theoretische Informatik (MSc-Inf-Theo) erbracht werden."
                    , "        Über die Zuordnung von Modulen zu diesem Bereich entscheidet der"
                    , "        Prüfungsausschuss nach Rücksprache mit den Dozentinnen und Dozenten des"
                    , "        Bereichs und macht diese in geeigneter Weise bekannt."
                    , "      #{numb:} Masterseminare zur Informatik (Msc-Inf-Sem) im Umfang von 5 LP: Ziel"
                    , "        eines Masterseminars ist eine eigenständige Auseinandersetzung mit"
                    , "        wissenschaftlichen Themen der Informatik, dem Schreiben wissenschaftlicher"
                    , "        Texte und dem Präsentieren wissenschaftlicher Ergebnisse."
                    , "      # Masterprojekte zur Informatik (Msc-Inf-Proj) im Umfang von 10 LP: Ziel"
                    , "        eines Masterprojekts ist die intensive, praktische Auseinandersetzung mit"
                    , "        einem aktuellen Thema der Informatik. Inhalt sollen insbesondere die"
                    , "        Bereiche Problemanalyse, Spezifikation und Implementierung sein."
                    , "        Das Masterprojekt soll in der Regel als Gruppenarbeit erfolgen,"
                    , "        so dass neben den fachlichen Inhalten auch Aspekte der Gruppen-"
                    , "        und Projektarbeit erlernt werden. Die Ergebnisse des Masterprojekts"
                    , "        werden im Rahmen eines Vortrags präsentiert."
                    , "      # Projektgruppen (Msc-Winf-PrGrp) im Umfang von 15 bis 20 LP:"
                    , "        Die Projektgruppe verfolgt dieselben Ziele wie ein Masterprojekt in"
                    , "        einem größeren Rahmen."
                    , "      #{nume:} Forschungsprojekte (MSc-WInf-FoPro, Mitarbeit in einer Arbeitsgruppe),"
                    , "        jeweils im Umfang von bis zu 10 LP."
                    , "Es müssen die folgenden Bedingungen bei den Modulen gemäß den Buchstaben {:numb}"
                    , "bis {:nume} eingehalten werden:"
                    , "  # 1"
                    , "      # a"
                    , "          # Ein oder zwei Masterseminare."
                    , "          # Ein oder zwei Masterprojekte oder eine Projektgruppe."
                    , "          # Höchstens ein Forschungsprojekt."
                    , "          # Der Gesamtumfang der Module gemäß den Buchstaben {:numb} bis {:nume} ist höchstens 25 LP."
                    , "  #{num2:} Außerfachlicher Wahlbereich im Umfang von 10 bis 15 LP: In diesem Bereich können"
                    , "    Studierende Module aus dem Angebot der Christian-Albrechts-Universität zu Kiel"
                    , "    wählen, welche nicht auch in einem anderen Bereich dieses Studiengangs belegt"
                    , "    werden können. Sprachkurse, welche nicht über das Niveau der gymnasialen Oberstufe"
                    , "    hinausgehen, oder die Muttersprache betreffen, sowie Module mit informatischem"
                    , "    beziehungsweise wirtschaftsinformatischem Inhalt, gehören nicht zu diesem Bereich."
                    , "    Bei der Wahl außerfachlicher Module müssen die Kapazitätsbeschränkungen anderer"
                    , "    Fächer gemäß § 9 Absatz 3 PVO beachtet werden."
                    , "  # Neben der Wahl von Modulen gemäß Nummer {:num2} ist im außerfachlichen Wahlbereich auch"
                    , "    die Wahl eines koordinierten Nebenfachs möglich. Das Nebenfach kann sowohl"
                    , "    konsekutiven Charakter haben und das gleiche Nebenfach aus dem Bachelorstudiengang"
                    , "    fortsetzen als auch ein neues einführendes Nebenfach sein. Die möglichen Nebenfächer"
                    , "    mit den zu absolvierenden Modulen werden zum Studienbeginn in geeigneter Weise durch"
                    , "    das Institut für Informatik bekannt gemacht. Dabei kann es in einzelnen Nebenfächern"
                    , "    erforderlich sein, mehr als 10 LP zu erreichen. Weitere Nebenfächer können in"
                    , "    Absprache mit den beteiligten Fächern und dem Prüfungsausschuss Informatik, bestimmt"
                    , "    werden. Das Nebenfach wird auf dem Zeugnis ausgewiesen."
                    , ""
                    , "Im Rahmen des Masterstudiums ist eine Masterarbeit im Umfang von 30 LP anzufertigen."
                    , "Näheres regelt § {:par6}. Die Masterarbeit erfolgt in der Regel als Abschluss des Masterstudiums."
                    , ""
                    , "Aktualisierungen der Wahlpflichtbereiche nimmt der Prüfungsausschuss vor;"
                    , "vor Einführung eines neuen Moduls werden die durchführenden Lehrpersonen und"
                    , "die Studiengangskoordinatorin oder der Studiengangskoordinator gehört."
                    , ""
                    , "Bereits für einen Bachelorabschluss verwendete Wahlpflichtmodule können"
                    , "nicht erneut eingebracht werden."
                    ]

    _ <-
        machText paragraph3 Nothing $
            pack $
                unlines
                    [ "§ Studienaufbau"
                    , ""
                    , "Die Regelstudienzeit und das Studienvolumen betragen drei Jahre beziehungsweise"
                    , "180 Leistungspunkte und circa 120 Semesterwochenstunden."
                    , ""
                    , "Der Bachelorstudiengang Wirtschaftsinformatik (Studienverlaufsplan siehe Anlage {:verlaufsplan},"
                    , "Module mit Prüfungsleistungen siehe Anlage {:anlage11} und {:anlage12}) setzt sich aus folgenden"
                    , "Bereichen zusammen:"
                    , "  # Grundmodule (Pflicht, BSc-WInf-G) im Umfang von 46 LP:"
                    , "      # - infEWInf-01a: Einführung in die Wirtschaftsinformatik (7 LP)"
                    , "      # - infEInf-01a: Einführung in die Informatik (8 LP)"
                    , "      # - Inf-InfRecht: Informatikrecht (2 LP)"
                    , "  # Aufbaumodule (Pflicht, BSc-WInf-A) im Umfang von 32 LP:"
                    , "      # - VWL-STATWX: Statistische Methoden (10 LP)"
                    , "      # - infST-01a: Softwaretechnik (7 LP)"
                    , "      # - infEthik-01a: Ethik in der Informatik (2 LP)"
                    , "  # Wahlpflichtmodule (BSc-WInf-WP-WInf und BSc-WInf-WP-Inf) im Umfang von 37 LP."
                    , "    Weitere Informationen sind in Anlage {:anlage12} aufgeführt. Diese Module bestehen in der"
                    , "    Regel aus einer Vorlesung mit einer begleitenden Übung."
                    , "    Bei der Wahl der Module müssen die Studierenden mindestens 23 LP aus dem Bereich"
                    , "    Wahlpflichtmodule BSc-WInf-WP-WInf und mindestens 7 LP aus dem Bereich"
                    , "    Wahlpflichtmodule BSc-WInf-WP-Inf wählen."
                    , "  # ein Projekt Wirtschaftsinformatik (BSc-WInf-Proj) im Umfang von 6 LP."
                    , "  # ein Seminarmodul zur Wirtschaftsinformatik (BSc-WInf-Sem) im Umfang von 7 LP,"
                    , "    gemäß Anlage {:anlage12}."
                    , "  # Studienangebote BWL oder VWL im Umfang von 40 LP:"
                    , "    Die Studienangebote BWL und VWL sind Alternativen. Mit der Wahl des ersten Moduls,"
                    , "    welches nicht in beiden Varianten vorkommt, legen Studierende fest, welche"
                    , "    Variante sie wählen. Ein Wechsel zu dem jeweils anderen Studienangebot ist"
                    , "    jederzeit möglich. Zum Erreichen des Bachelorabschlusses müssen alle Module"
                    , "    eines der beiden Studienangebote erfolgreich absolviert werden."
                    , "      # Studienangebot BWL, gemäß Anlage {:anlage6}:"
                    , "          # BWL-EinfBWL: Einführung in die Betriebswirtschaftslehre (5 LP)"
                    , "          # BWL-ERW: Externes Rechnungswesen (5 LP)"
                    , "          # BWL-ER: Entscheidungsrechnungen (5 LP)"
                    , "          # VWL-EVWL: Einführung in die Volkswirtschaftslehre"
                    , "          # BWL-InnoMProz: Innovationsmanagement: Prozesse und Methoden (5 LP)"
                    , "          # Zwei Module des Wahlpflichtbereichs BWL (zusammen 10 LP):"
                    , "              # - BWL-Ent: Decision Analysis I (5 LP)"
                    , "              # - BWL-RDM: Decision Analysis II (5 LP)"
                    , "              # - BWL-ProdLog: Produktion und Logistik (5 LP)"
                    , "              # - BWL-Mark: Marketing (5 LP)"
                    , "            Wurde ein Wahlpflichtmodul erfolgreich abgeschlossen, darf dieses"
                    , "            nicht durch ein anderes Wahlpflichtmodul (zum Beispiel zur"
                    , "            Notenverbesserung) ersetzt werden."
                    , "      # Studienangebot VWL, gemäß Anlage {:anlage6}:"
                    , "          # BWL-EinfBWL: Einführung in die Betriebswirtschaftslehre (5 LP)"
                    , "          # VWL-EVWL: Einführung in die Volkswirtschaftslehre (10 LP)"
                    , "          # VWLvwlMikro1-01a: Grundzüge der mikroökonomischen Theorie I (5 LP)"
                    , "  # Bachelorarbeit, individuell oder im Abschlussprojekt, im Umfang von 12 LP gemäß § {:abschluss}."
                    ]
    _ <- machText paragraph4 Nothing "Und noch ein Abschnitt."
    _ <- machText paragraph5 Nothing "erixx"
    _ <-
        machText paragraph1 parent $
            pack $
                unlines
                    [ "§ Zugang zum Masterstudium"
                    , ""
                    , "Der Zugang zum Studiengang setzt voraus, dass die Bewerberin oder der Bewerber"
                    , "einen anerkannten, qualifizierten Bachelor of Science in Informatik oder einer"
                    , "verwandten Disziplin besitzt. Der Zugang kann nur erfolgen, wenn"
                    , "  # mit diesem Abschluss dieselben Lernziele erreicht werden, die"
                    , "    mit dem Bachelor of Science Informatik der Christian-Albrechts-Universität"
                    , "    zu Kiel erreicht werden, oder"
                    , "  # gemäß der Satzung zur Feststellung der Eignung für den 1-Fach-Masterstudiengang"
                    , "    Informatik die Eignung festgestellt wurde."
                    , ""
                    , "Die Feststellung, ob die Zugangsvoraussetzungen erfüllt sind, insbesondere, ob ein"
                    , "qualifizierter Abschluss vorliegt, trifft der Prüfungsausschuss Informatik."
                    , "Erfüllt die Studiengangskoordinatorin, der Studiengangskoordinator oder eine"
                    , "andere vom Prüfungsausschuss beauftragte Person die in der Anerkennungssatzung"
                    , "für eine Feststellung genannten Voraussetzungen, entscheidet diese oder dieser"
                    , "über das Vorliegen der Zugangsvoraussetzungen."
                    , ""
                    , "Der Zugang zum Studiengang Informatik mit dem Abschluss Master of Science setzt"
                    , "weiter den Nachweis von Englischkenntnissen voraus durch"
                    , "  # eine allgemeine Hochschulreife oder"
                    , "  # eine fachgebundene Hochschulreife mit mindestens drei Jahren Englisch auf"
                    , "    grundlegendem Niveau (Grundkursniveau) oder"
                    , "  # entsprechende Sprachnachweise auf dem Niveau B1 des Gemeinsamen Europäischen"
                    , "    Referenzrahmens für Sprachen (GER) oder"
                    , "  # ein Ergebnis von mindestens 145 Punkten im Abschnitt „Verbal Reasoning“"
                    , "    eines GRE®revised General Tests."
                    , "Absolventinnen und Absolventen des CAU-Bachelorstudiengangs, auf dem dieser"
                    , "Master aufbaut und für den die geforderten Voraussetzungen bereits nachzuweisen"
                    , "waren, müssen diese Voraussetzungen nicht erneut nachweisen."
                    ]
    --
    _ <- machText anlage1 Nothing "Bester Laden"
    _ <- machText anlage2 Nothing "Kein guter Funkverkehr"
    _ <- machText anlage3 Nothing "Sehr schön, vor allem im Vergleich zu Behringer"
    _ <-
        machText anlage4 Nothing "Hätt ich auch gern, hab leider nur 10 Jahre alte AD"
    _ <- machText anlage5 Nothing "Keine Ahnung hab vergessen"
    return ()
  where
    withDB x = do
        Right (Right y) <- x db
        return y
