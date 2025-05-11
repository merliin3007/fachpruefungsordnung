{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Database (getConnection)
import qualified Hasql.Session as Session
import Server
import Versioning.Commit
import qualified Versioning.Sessions as Sessions
import Versioning.Tree

testTree :: Tree NodeWithMaybeRef
testTree =
  mkTree
    (Node "document" (Just "Hier könnten ihre Metadaten stehen."))
    [ mkEdge
        "Abschnitt 1"
        (mkTree (Node "abschnitt" (Just "Das hier ist ein Abschnitt.")) []),
      mkEdge
        "Abschnitt 2"
        (mkTree (Node "abschnitt" (Just "Das hier ist noch ein Abschnitt.")) []),
      mkEdge
        "Abschnitt 3"
        (mkTree (Node "abschnitt" (Just "Auch das hier ist ein Abschnitt.")) []),
      mkEdge
        "Abschnitt 4"
        (mkTree (Node "abschnitt" (Just "Und noch ein Abschnitt.")) []),
      mkEdge
        "Anlagen"
        ( mkTree
            (Node "anlagen" Nothing)
            [ mkEdge
                "Anlage 1"
                (mkTree (Node "anlage" (Just "Das hier ist eine Anlage.")) []),
              mkEdge
                "Anlage 2"
                (mkTree (Node "anlage" (Just "Das hier ist auch eine Anlage.")) []),
              mkEdge
                "Anlage 3"
                (mkTree (Node "anlage" (Just "Guck mal! Noch eine Anlage!")) [])
            ]
        )
    ]

testCommit :: CreateCommit
testCommit =
  CreateCommit
    (CommitInfo 1 (Just "Test Commit") Nothing)
    (Value testTree)

someFunc :: IO ()
someFunc = do
  Right connection <- getConnection
  -- Datenbank zumüllen :)
  commit <- Session.run (Sessions.createCommit testCommit) connection
  print commit
  runServer
  return ()
