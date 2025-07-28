{-# LANGUAGE OverloadedStrings #-}

module Docs.TestDoc (createTestDocument, createTextElement) where

import Hasql.Connection (Connection)

import Data.Maybe (fromMaybe)
import Data.UUID (fromString)

import Docs
import qualified Docs.Document as Document
import Docs.Hasql.Database (run, runTransaction)
import Docs.TextElement (TextElementRef (..))
import qualified Docs.TextElement as TextElement
import Docs.TextRevision (NewTextRevision (NewTextRevision))
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
    let textRevision element = NewTextRevision (textElementRef element) Nothing
    let machText = ((withDB . runTransaction . createTextRevision userID) .) . textRevision
    _ <- machText paragraph1 "Das hier ist ein Abschnitt."
    _ <- machText paragraph2 "Das hier ist noch ein Abschnitt."
    _ <- machText paragraph3 "Auch das hier ist ein Abschnitt."
    _ <- machText paragraph4 "Und noch ein Abschnitt."
    _ <- machText paragraph5 "erixx"
    --
    _ <- machText anlage1 "Bester Laden"
    _ <- machText anlage2 "Kein guter Funkverkehr"
    _ <- machText anlage3 "Sehr schön, vor allem im Vergleich zu Behringer"
    _ <- machText anlage4 "Hätt ich auch gern, hab leider nur 10 Jahre alte AD"
    _ <- machText anlage5 "Keine Ahnung hab vergessen"
    return ()
  where
    withDB x = do
        Right (Right y) <- x db
        return y
