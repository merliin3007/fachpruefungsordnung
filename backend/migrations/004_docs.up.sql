CREATE TABLE IF NOT EXISTS docs (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    "group" INTEGER NOT NULL REFERENCES groups (id)
);

CREATE TABLE IF NOT EXISTS doc_text_elements (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    kind TEXT NOT NULL,
    document INTEGER REFERENCES docs (id)
);

CREATE TABLE IF NOT EXISTS doc_text_revisions (
    id INTEGER PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    text_element INTEGER NOT NULL REFERENCES doc_text_elements (id),
    creation_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    author UUID NOT NULL REFERENCES users (id),
    content TEXT NOT NULL
);

CREATE INDEX CONCURRENTLY IF NOT EXISTS doc_text_revisions_timestamp_index ON doc_text_revisions (creation_ts DESC);

CREATE TABLE IF NOT EXISTS doc_tree_nodes (
    hash BYTEA PRIMARY KEY NOT NULL, -- hash over metadata and children
    kind TEXT NOT NULL,
    type TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS doc_tree_edges (
    parent BYTEA NOT NULL REFERENCES doc_tree_nodes (hash),
    position INTEGER NOT NULL,
    title TEXT NOT NULL,
    child_node BYTEA REFERENCES doc_tree_nodes (hash),
    child_text_element INTEGER REFERENCES doc_text_elements (id),
    PRIMARY KEY (parent, position),
    CHECK (
        (
            child_node IS NOT NULL
            AND child_text IS NULL
        )
        OR (
            child_node IS NULL
            AND child_text IS NOT NULL
        )
    )
);

CREATE TABLE IF NOT EXISTS doc_tree_revisions (
    id INTEGER PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    document INTEGER NOT NULL REFERENCES docs (id),
    creation_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    author UUID NOT NULL REFERENCES users (id),
    root BYTEA NOT NULL REFERENCES doc_tree_nodes (hash)
);

CREATE INDEX CONCURRENTLY IF NOT EXISTS doc_tree_revisions_timestamp_index ON doc_tree_revisions (creation_ts DESC);

CREATE OR REPLACE VIEW doc_revisions AS
    SELECT
        a.text_element,
        a.id,
        a.creation_ts,
        a.author
    FROM doc_text_revisions a
    UNION ALL
    SELECT
        NULL as text_element,
        b.id,
        b.creation_ts,
        b.author
    FROM doc_tree_revisions b;
