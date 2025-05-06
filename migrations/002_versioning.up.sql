CREATE TABLE blobs (
    hash TEXT PRIMARY KEY NOT NULL,
    content TEXT NOT NULL -- compressed content
);

CREATE TABLE versions (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    creation_ts TIMESTAMP NOT NULL DEFAULT NOW (),
    author INTEGER NOT NULL REFERENCES users (id)
);

CREATE TABLE objects (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    kind TEXT NOT NULL
);

CREATE TABLE object_versions (
    hash TEXT PRIMARY KEY NOT NULL, -- hash over content and child hashes
    object INTEGER NOT NULL REFERENCES objects (id),
    content TEXT REFERENCES blobs (hash)
);

CREATE TABLE trees (
    child TEXT NOT NULL REFERENCES object_versions (hash),
    parent TEXT NOT NULL REFERENCES object_versions (hash),
    child_position INTEGER NOT NULL,
    PRIMARY KEY (child),
    UNIQUE (parent, child_position)
);

CREATE TABLE commits (
    version INTEGER PRIMARY KEY NOT NULL REFERENCES versions (id),
    message TEXT NOT NULL,
    root TEXT NOT NULL REFERENCES object_versions (hash),
    parent INTEGER REFERENCES commits (version)
);

CREATE TABLE documents (
    id INTEGER GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL,
    head INTEGER NOT NULL REFERENCES commits (version)
);
