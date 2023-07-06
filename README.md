# WhichIsBetterAI - API Documentation

Welcome to WhichIsBetterAI's API documentation. This program utilizes AI models to compare random concepts pulled from Wikipedia and then adjusts their ELO scores based on the outcome. These adjustments are used to create a fun and innovative leaderboard.

## Main Features

- **Add new Concepts:** You can add new concepts from Wikipedia, including the name, description and the Wikipedia link.
- **Add new Models:** You can add new AI models for comparisons, including the model name and description.
- **Run Random Comparisons:** You can run random comparisons using AI models between different concepts.
- **Get ELO scores:** You can retrieve ELO scores for different concepts.
- **Get Leaderboard:** You can retrieve a leaderboard for different concepts based on their ELO scores.

## Data Model

The application uses a relational data model with four main tables:

- `concepts`: This table holds the details about the concepts pulled from Wikipedia.
- `ai_models`: This table holds the details about the AI models used for comparisons.
- `comparison_results`: This table holds the results of the comparisons run by the AI models.
- `elo_scores`: This table holds the ELO scores of the concepts.

## Endpoints

Our API offers various endpoints for interacting with the data:

- `/get-elos`: Retrieves ELO scores.
- `/get-leaderboard`: Retrieves the leaderboard.
- `/add-model`: Adds a new AI model.
- `/get-models`: Retrieves all AI models.
- `/get-comparisons`: Retrieves all comparisons.
- `/run-random-comparisons`: Runs random comparisons.
- `/add-concept`: Adds a new concept.
- `/get-concepts`: Retrieves all concepts.

## Instructions for use

The API endpoints can be used in any application that can make HTTP requests. For example, you can use the `fetch` function in JavaScript to make GET and POST requests to these endpoints.

## PopulateDB.hs - Automatic Population of Database

This Haskell script is designed to automatically fetch random articles from Wikipedia, extract relevant information, and add them as concepts in the database.

### Key Functions

- `fetchRandomArticle`: Fetches a random article summary from Wikipedia.
- `login`: Logs into the local server with a provided username and password.
- `addConcept`: Adds a new concept to the database using the given article information.

### How to Run the Script

To run the script, use `stack ghci PopulateDB.hs` from the command line in the directory where `PopulateDB.hs` is located. This starts an interactive GHC session with `PopulateDB.hs` loaded.

After the GHC session is started, simply run the `populateDB` function with the command `populateDB`.

This script will automatically fetch and add 2000 random Wikipedia articles as concepts in the database. You can change this number in the `populateDB` function.

