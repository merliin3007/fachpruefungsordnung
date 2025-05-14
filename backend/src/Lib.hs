{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    )
where

import Database (getConnection, migrate)
import qualified Hasql.Session as Session
import Hasql.Transaction (statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )
import Server
import UserManagement.Statements as UStatements
import Versioning.Commit
import Versioning.Transactions as VTransactions
import Versioning.Tree

-- a good example document with example content and example structure
testTree :: Tree NodeWithMaybeRef
testTree =
    mkTree
        (Node "document" (Just "Hier könnten ihre Metadaten stehen."))
        [ mkEdge
            "Abschnitt 1"
            (mkTree (Node "abschnitt" (Just "Das hier ist ein Abschnitt.")) [])
        , mkEdge
            "Abschnitt 2"
            (mkTree (Node "abschnitt" (Just "Das hier ist noch ein Abschnitt.")) [])
        , mkEdge
            "Abschnitt 3"
            (mkTree (Node "abschnitt" (Just "Auch das hier ist ein Abschnitt.")) [])
        , mkEdge
            "Abschnitt 4"
            (mkTree (Node "abschnitt" (Just "Und noch ein Abschnitt.")) [])
        , mkEdge
            "Anlagen"
            ( mkTree
                (Node "anlagen" Nothing)
                [ mkEdge
                    "Anlage 1"
                    (mkTree (Node "anlage" (Just "Das hier ist eine Anlage.")) [])
                , mkEdge
                    "Anlage 2"
                    (mkTree (Node "anlage" (Just "Das hier ist auch eine Anlage.")) [])
                , mkEdge
                    "Anlage 3"
                    (mkTree (Node "anlage" (Just "Guck mal! Noch eine Anlage!")) [])
                ]
            )
        ]

testCommit :: Session.Session ExistingCommit
testCommit = transaction Serializable Write $ do
    userId <- statement "test@test.com" UStatements.getUserID
    let commit =
            CreateCommit (CommitInfo userId (Just "Test Commit") Nothing) (Value testTree)
    VTransactions.createCommit commit

someFunc :: IO ()
someFunc = do
    Right connection <- getConnection
    Right _ <- migrate connection
    -- Datenbank zumüllen :)
    commit <- Session.run testCommit connection
    print commit
    runServer
    return ()
