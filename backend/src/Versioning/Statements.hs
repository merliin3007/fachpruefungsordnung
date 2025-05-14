{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Versioning.Statements
  ( createNode,
    getNodeKind,
    putVersion,
    putEdge,
    createCommit,
    getCommit,
    getChildrenByParentHash,
    getVersion,
  )
where

import           Data.Profunctor   (lmap, rmap)
import           Data.Text
import           Data.Vector
import           Hasql.Statement
import           Hasql.TH
import           Versioning.Commit
import           Versioning.Hash   (Hash (..), Hashed (..), hashToBS)
import           Versioning.Tree

createNode :: Statement Text NodeID
createNode =
  rmap
    NodeID
    [singletonStatement|
    insert into nodes (kind) values ($1 :: text) returning id :: int4
  |]

getNodeKind :: Statement NodeID Text
getNodeKind =
  lmap
    nodeIDInt32
    [singletonStatement|
    select kind :: text from nodes where id = $1 :: int4
  |]

putVersion :: Statement (Hashed NodeWithRef) ()
putVersion =
  lmap
    ( \(Hashed (Hash hash) (NodeWithRef (NodeID ref) node)) ->
        (hash, ref, nodeContent node)
    )
    [resultlessStatement|
      insert into node_versions (hash, node, content)
      values ($1 :: bytea, $2 :: int4, $3 :: text?)
      on conflict (hash) do nothing
    |]

getVersion :: Statement Hash (Hashed NodeWithRef)
getVersion =
  rmap
    ( \(hash, node, kind, content) ->
        Hashed (Hash hash) (NodeWithRef (NodeID node) (Node kind content))
    )
    $ lmap
      (\(Hash bs) -> bs)
      [singletonStatement|
      select
        hash :: bytea,
        node :: int4,
        kind :: text,
        content :: text?
      from
        node_versions
        join nodes on node = id
      where
        hash = $1 :: bytea
    |]

putEdge :: Statement DataEdge ()
putEdge =
  lmap
    ( \(DataEdge parent child position title) ->
        (hashToBS parent, hashToBS child, position, title)
    )
    [resultlessStatement|
      insert into trees (parent, child, child_position, child_title)
      values ($1 :: bytea, $2 :: bytea, $3 :: int4, $4 :: text)
    |]

getChildrenByParentHash :: Statement Hash (Vector (Text, Hashed NodeWithRef))
getChildrenByParentHash =
  rmap
    ( fmap
        ( \(title, hash, node, kind, content) ->
            (title, Hashed (Hash hash) (NodeWithRef (NodeID node) (Node kind content)))
        )
    )
    $ lmap
      (\(Hash bs) -> bs)
      [vectorStatement|
    select
      child_title :: text,
      hash :: bytea,
      node :: int4,
      kind :: text,
      content :: text?
    from
      node_versions
      join trees on hash = child
      join nodes on node = id
    where
      parent = $1 :: bytea
    order by child_position asc
  |]

createCommit :: Statement CommitBody CommitHeader
createCommit =
  rmap (\(newID, ts) -> CommitHeader (CommitID newID) ts) $
    lmap
      ( \(CommitBody info root) ->
          ( commitAuthor info,
            commitMessage info,
            hashToBS $ treeRefHash root,
            commitIDInt32 . commitRefID <$> parentCommit info
          )
      )
      [singletonStatement|
      insert into commits (author, message, root, parent)
      values ($1 :: uuid, $2 :: text?, $3 :: bytea, $4 :: int4?)
      returning id :: int4, creation_ts :: timestamp
    |]

getCommit :: Statement CommitID ExistingCommit
getCommit =
  rmap
    ( \(ref, ts, author, message, root, parent) ->
        ExistingCommit
          (CommitHeader (CommitID ref) ts)
          ( CommitBody
              (CommitInfo author message (Ref . CommitID <$> parent))
              (Ref (Hash root))
          )
    )
    $ lmap
      (\(CommitID i) -> i)
      [singletonStatement|
        select
          id :: int4,
          creation_ts :: timestamp,
          author :: uuid,
          message :: text?,
          root :: bytea,
          parent :: int4?
        from
          commits
        where
          id = $1 :: int4
    |]
