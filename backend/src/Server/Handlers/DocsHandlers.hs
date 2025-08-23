{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.DocsHandlers
    ( DocsAPI
    , docsServer
    ) where

import Data.Time (UTCTime)

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as LBS

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session

import Servant
    ( Capture
    , Get
    , Handler
    , JSON
    , Post
    , QueryParam
    , ReqBody
    , Server
    , err400
    , err403
    , err500
    , errBody
    , throwError
    , type (:<|>) (..)
    , type (:>)
    )
import Servant.Auth.Server (Auth, AuthResult (Authenticated))

import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil (errNotLoggedIn, tryGetDBConnection)

import UserManagement.User (UserID)

import qualified Docs
import Docs.Document (Document, DocumentID (..))
import Docs.DocumentHistory (DocumentHistory)
import Docs.Hasql.Database (run, runTransaction)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementRef (..)
    , prettyPrintTextElementRef
    )
import Docs.TextRevision
    ( ConflictStatus
    , NewTextRevision (..)
    , TextElementRevision
    , TextRevisionHistory
    , TextRevisionRef (..)
    , TextRevisionSelector
    , prettyPrintTextRevisionRef
    )
import Docs.Tree (Node)
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    , TreeRevisionSelector
    , prettyPrintTreeRevisionRef
    )

import Docs.Comment
    ( Comment
    , CommentID
    , CommentRef (CommentRef)
    , Message
    , prettyPrintCommentRef
    )
import Server.DTOs.Comments (Comments (Comments))
import Server.DTOs.CreateComment (CreateComment)
import qualified Server.DTOs.CreateComment as CreateComment
import Server.DTOs.CreateDocument (CreateDocument)
import qualified Server.DTOs.CreateDocument as CreateDocument
import Server.DTOs.CreateReply (CreateReply)
import qualified Server.DTOs.CreateReply as CreateReply
import Server.DTOs.CreateTextElement (CreateTextElement)
import qualified Server.DTOs.CreateTextElement as CreateTextElement
import Server.DTOs.CreateTextRevision (CreateTextRevision)
import qualified Server.DTOs.CreateTextRevision as CreateTextRevision
import Server.DTOs.Documents
    ( Documents (Documents)
    , DocumentsQuery (DocumentsQuery)
    )
import qualified Server.DTOs.Documents as Documents
import Server.Handlers.RenderHandlers (RenderAPI, renderServer)
import UserManagement.Group (GroupID)

type DocsAPI =
    "docs"
        :> ( {-   -} PostDocument
                :<|> GetDocument
                :<|> GetDocuments
                :<|> PostTextElement
                :<|> PostTextRevision
                :<|> GetTextElementRevision
                :<|> PostTreeRevision
                :<|> GetTreeRevision
                :<|> GetTreeRevisionFull
                :<|> GetTextHistory
                :<|> GetTreeHistory
                :<|> GetDocumentHistory
                :<|> PostComment
                :<|> GetComments
                :<|> ResolveComment
                :<|> PostReply
                :<|> RenderAPI
           )

type PostDocument =
    Auth AuthMethod Auth.Token
        :> ReqBody '[JSON] CreateDocument
        :> Post '[JSON] Document

type GetDocument =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> Get '[JSON] Document

type GetDocuments =
    Auth AuthMethod Auth.Token
        :> QueryParam "user" UserID
        :> QueryParam "group" GroupID
        :> Get '[JSON] Documents

type PostTextElement =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> ReqBody '[JSON] CreateTextElement
        :> Post '[JSON] TextElement

type PostTextRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "rev"
        :> ReqBody '[JSON] CreateTextRevision
        :> Post '[JSON] ConflictStatus

type GetTextElementRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "rev"
        :> Capture "textRevision" TextRevisionSelector
        :> Get '[JSON] (Maybe TextElementRevision)

type PostTreeRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> ReqBody '[JSON] (Node TextElementID)
        :> Post '[JSON] (TreeRevision TextElementID)

type GetTreeRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> Capture "treeRevision" TreeRevisionSelector
        :> Get '[JSON] (Maybe (TreeRevision TextElement))

type GetTreeRevisionFull =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> Capture "treeRevision" TreeRevisionSelector
        :> "full"
        :> Get '[JSON] (Maybe (TreeRevision TextElementRevision))

type GetTextHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "history"
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Docs.Limit
        :> Get '[JSON] TextRevisionHistory

type GetTreeHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> "history"
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Docs.Limit
        :> Get '[JSON] TreeRevisionHistory

type GetDocumentHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "history"
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Docs.Limit
        :> Get '[JSON] DocumentHistory

type PostComment =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> ReqBody '[JSON] CreateComment
        :> Post '[JSON] Comment

type GetComments =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> Get '[JSON] Comments

-- | jaja, das ist kein cleanes rest design,
--   aber das ist mir langsam auch wirklich scheiß egal.
type ResolveComment =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> Capture "commentID" CommentID
        :> "resolve"
        :> Post '[JSON] ()

type PostReply =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> Capture "commentID" CommentID
        :> "replies"
        :> ReqBody '[JSON] CreateReply
        :> Post '[JSON] Message

docsServer :: Server DocsAPI
docsServer =
    {-    -} postDocumentHandler
        :<|> getDocumentHandler
        :<|> getDocumentsHandler
        :<|> postTextElementHandler
        :<|> postTextRevisionHandler
        :<|> getTextElementRevisionHandler
        :<|> postTreeRevisionHandler
        :<|> getTreeRevisionHandler
        :<|> getTreeRevisionFullHandler
        :<|> getTextHistoryHandler
        :<|> getTreeHistoryHandler
        :<|> getDocumentHistoryHandler
        :<|> postCommentHandler
        :<|> getCommentsHandler
        :<|> resolveCommentHandler
        :<|> createReplyHandler
        :<|> renderServer

postDocumentHandler
    :: AuthResult Auth.Token
    -> CreateDocument
    -> Handler Document
postDocumentHandler auth doc = do
    userID <- getUser auth
    withDB $
        run $
            Docs.createDocument
                userID
                (CreateDocument.groupID doc)
                (CreateDocument.title doc)

getDocumentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Handler Document
getDocumentHandler auth docID = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocument userID docID

getDocumentsHandler
    :: AuthResult Auth.Token
    -> Maybe UserID
    -> Maybe GroupID
    -> Handler Documents
getDocumentsHandler auth byUserID byGroupID = do
    userID <- getUser auth
    result <- withDB $ run $ Docs.getDocuments userID byUserID byGroupID
    return $
        Documents
            { Documents.documents = result
            , Documents.query =
                DocumentsQuery
                    { Documents.user = byUserID
                    , Documents.group = byGroupID
                    }
            }

postTextElementHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> CreateTextElement
    -> Handler TextElement
postTextElementHandler auth docID element = do
    userID <- getUser auth
    withDB $
        run $
            Docs.createTextElement
                userID
                docID
                (CreateTextElement.kind element)

postTextRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CreateTextRevision
    -> Handler ConflictStatus
postTextRevisionHandler auth docID textID revision = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.createTextRevision userID $
                NewTextRevision
                    (TextElementRef docID textID)
                    (CreateTextRevision.parent revision)
                    (CreateTextRevision.content revision)
                    (CreateTextRevision.commentAnchors revision)

getTextElementRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> TextRevisionSelector
    -> Handler (Maybe TextElementRevision)
getTextElementRevisionHandler auth docID textID revision = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTextElementRevision userID $
                TextRevisionRef (TextElementRef docID textID) revision

postTreeRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Node TextElementID
    -> Handler (TreeRevision TextElementID)
postTreeRevisionHandler auth docID node = do
    userID <- getUser auth
    withDB $ runTransaction $ Docs.createTreeRevision userID docID node

getTreeRevisionFullHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TreeRevisionSelector
    -> Handler (Maybe (TreeRevision TextElementRevision))
getTreeRevisionFullHandler auth docID revision = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTreeWithLatestTexts userID $
                TreeRevisionRef docID revision

getTreeRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TreeRevisionSelector
    -> Handler (Maybe (TreeRevision TextElement))
getTreeRevisionHandler auth docID revision = do
    userID <- getUser auth
    withDB $ run $ Docs.getTreeRevision userID $ TreeRevisionRef docID revision

getTextHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler TextRevisionHistory
getTextHistoryHandler auth docID textID before limit = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTextHistory
                userID
                (TextElementRef docID textID)
                before
                limit

getTreeHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler TreeRevisionHistory
getTreeHistoryHandler auth docID before limit = do
    userID <- getUser auth
    withDB $ run $ Docs.getTreeHistory userID docID before limit

getDocumentHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler DocumentHistory
getDocumentHistoryHandler auth docID before limit = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocumentHistory userID docID before limit

postCommentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CreateComment
    -> Handler Comment
postCommentHandler auth docID textID comment = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.createComment
                userID
                (TextElementRef docID textID)
                (CreateComment.text comment)

getCommentsHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Handler Comments
getCommentsHandler auth docID textID = do
    userID <- getUser auth
    comments <-
        withDB $
            run $
                Docs.getComments
                    userID
                    (TextElementRef docID textID)
    return $ Comments comments

resolveCommentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CommentID
    -> Handler ()
resolveCommentHandler auth docID textID commentID = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.resolveComment userID (CommentRef (TextElementRef docID textID) commentID)

createReplyHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CommentID
    -> CreateReply
    -> Handler Message
createReplyHandler auth docID textID commentID bodyDTO = do
    userID <- getUser auth
    withDB
        $ runTransaction
        $ Docs.createReply
            userID
            (CommentRef (TextElementRef docID textID) commentID)
        $ CreateReply.text
            bodyDTO

-- utililty

getUser :: AuthResult Auth.Token -> Handler UserID
getUser (Authenticated Auth.Token {..}) = return subject
getUser _ = throwError errNotLoggedIn

withDB
    :: (Connection -> IO (Either Session.SessionError (Docs.Result a)))
    -> Handler a
withDB io = do
    db <- tryGetDBConnection
    result <- liftIO $ io db
    guardedDBAccess result

guardedDBAccess
    :: Either Session.SessionError (Docs.Result a)
    -> Handler a
guardedDBAccess result = guardDBResult result >>= guardDocsResult

guardDBResult :: Either Session.SessionError a -> Handler a
guardDBResult (Right ok) = return ok
guardDBResult (Left err) =
    throwError $
        err500
            { errBody = LBS.pack $ "Database error: " ++ show err ++ "\n"
            }

guardDocsResult :: Docs.Result a -> Handler a
guardDocsResult (Right ok) = return ok
guardDocsResult (Left err) = throwError $ mapErr err
  where
    mapErr (Docs.NoPermission docID perms) =
        err403
            { errBody =
                LBS.pack $
                    "You are not allowed to "
                        ++ show perms
                        ++ " document "
                        ++ show (unDocumentID docID)
                        ++ "!\n"
            }
    mapErr (Docs.NoPermissionForUser userID) =
        err403
            { errBody =
                LBS.pack $
                    "You are not allowed to view information about "
                        ++ show userID
                        ++ "!\n"
            }
    mapErr (Docs.NoPermissionInGroup groupID) =
        err403
            { errBody =
                LBS.pack $
                    "You are not an admin in group "
                        ++ show groupID
                        ++ "!\n"
            }
    mapErr (Docs.DocumentNotFound docID) =
        err400
            { errBody =
                LBS.pack $
                    "Document "
                        ++ show (unDocumentID docID)
                        ++ " not found!\n"
            }
    mapErr (Docs.TextElementNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TextElement "
                        ++ prettyPrintTextElementRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.TextRevisionNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TextRevision "
                        ++ prettyPrintTextRevisionRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.TreeRevisionNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TreeRevision "
                        ++ prettyPrintTreeRevisionRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.CommentNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "Comment "
                        ++ prettyPrintCommentRef ref
                        ++ " not found!\n"
            }
