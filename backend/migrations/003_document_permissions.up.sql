CREATE TYPE DocPermission AS ENUM('reader', 'reviewer', 'editor');

CREATE TABLE IF NOT EXISTS external_document_rights (
    user_id UUID NOT NULL,
    document_id INTEGER NOT NULL,
    permission DocPermission NOT NULL,
    PRIMARY KEY (user_id, document_id),
    FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    FOREIGN KEY (document_id) REFERENCES documents (id) ON DELETE CASCADE
);
