CREATE TABLE IF NOT EXISTS docs (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    group_id INTEGER NOT NULL REFERENCES groups (id)
);

CREATE TABLE IF NOT EXISTS doc_texts (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    kind TEXT NOT NULL,
    root INTEGER REFERENCES doc_texts (id)
);

CREATE TABLE IF NOT EXISTS doc_text_contents (
    hash BYTEA NOT NULL PRIMARY KEY,
    content TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS doc_text_versions (
    id INTEGER PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    text_id INTEGER NOT NULL REFERENCES doc_texts (id),
    creation_ts TIMESTAMPTZ NOT NULL,
    author_id UUID NOT NULL REFERENCES users (id),
    content BYTEA REFERENCES doc_text_contents (id)
);

CREATE INDEX ON doc_text_versions (creation_ts DESC);

CREATE TABLE IF NOT EXISTS doc_tree_nodes (
    id INTEGER PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    metadata BYTEA REFERENCES doc_text_contents (id)
);

CREATE TABLE IF NOT EXISTS doc_tree_edges (
    parent INTEGER NOT NULL REFERENCES doc_tree_nodes (id),
    position INTEGER NOT NULL,
    title TEXT NOT NULL,
    child_node INTEGER REFERENCES doc_tree_nodes (id),
    child_text INTEGER REFERENCES doc_texts (id),
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

CREATE TABLE IF NOT EXISTS doc_tree_versions (
    id INTEGER PRIMARY KEY NOT NULL GENERATED ALWAYS AS IDENTITY,
    document INTEGER NOT NULL REFERENCES docs (id),
    creation_ts TIMESTAMPTZ NOT NULL,
    author_id UUID NOT NULL REFERENCES users (id),
    root INTEGER NOT NULL REFERENCES doc_tree_nodes (id)
);

CREATE INDEX ON doc_tree_versions (creation_ts DESC);
