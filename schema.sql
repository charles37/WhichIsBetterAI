CREATE TABLE users
( id UUID PRIMARY KEY
, name TEXT NOT NULL UNIQUE
, password TEXT NOT NULL
);

CREATE TABLE contents
( id UUID PRIMARY KEY
, content TEXT NOT NULL
, user_id UUID REFERENCES users (id) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE tags
( id UUID PRIMARY KEY
, name TEXT NOT NULL UNIQUE
);

CREATE TABLE contents_tags
( content_id UUID REFERENCES contents (id) ON DELETE CASCADE ON UPDATE CASCADE
, tag_id     UUID REFERENCES tags     (id) ON DELETE CASCADE ON UPDATE CASCADE
);


-- Table for Concepts
CREATE TABLE concepts (
    concept_id UUID PRIMARY KEY,
    concept_name VARCHAR(255) NOT NULL,
    concept_description TEXT NOT NULL,
    concept_wiki_link TEXT NOT NULL UNIQUE
);

-- Table for AI Models
CREATE TABLE ai_models (
    model_id UUID PRIMARY KEY,
    model_name VARCHAR(255) NOT NULL,
    model_description TEXT NOT NULL
);

-- Table for Comparison Results
CREATE TABLE comparison_results (
    comparison_id UUID PRIMARY KEY,
    concept1_id UUID NOT NULL,
    concept2_id UUID NOT NULL,
    concept1_elo_before INTEGER NOT NULL,
    concept2_elo_before INTEGER NOT NULL,
    concept1_elo_after INTEGER NOT NULL,
    concept2_elo_after INTEGER NOT NULL,
    winning_concept_id UUID NOT NULL,
    model_id UUID NOT NULL,
    comparison_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (concept1_id) REFERENCES concepts (concept_id),
    FOREIGN KEY (concept2_id) REFERENCES concepts (concept_id),
    FOREIGN KEY (winning_concept_id) REFERENCES concepts (concept_id),
    FOREIGN KEY (model_id) REFERENCES ai_models (model_id)
);



-- Table for ELO Scores
CREATE TABLE elo_scores (
    elo_id UUID PRIMARY KEY,
    concept_id UUID NOT NULL,
    model_id UUID NOT NULL,
    elo_score INTEGER NOT NULL DEFAULT 0,
    last_update_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (concept_id) REFERENCES concepts (concept_id),
    FOREIGN KEY (model_id) REFERENCES ai_models (model_id)
);

