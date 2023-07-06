{-# LANGUAGE OverloadedStrings #-}

module PopulateDB where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Network.HTTP.Types.URI (urlEncode)

data Urls = Urls
  { desktop :: Text
  , mobile :: Text
  } deriving Show

instance FromJSON Urls where
  parseJSON = withObject "Urls" $ \v -> Urls
    <$> (v .: "desktop" >>= (.: "page"))
    <*> (v .: "mobile" >>= (.: "page"))

data Summary = Summary
  { title :: Text
  , extract :: Text
  , content_urls :: Urls
  } deriving Show

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \v -> Summary
    <$> v .: "title"
    <*> v .: "extract"
    <*> v .: "content_urls"

data Token = Token
  { token :: Text
  } deriving Show

instance FromJSON Token where
  parseJSON = withText "Token" $ \v -> 
    let stripped = T.dropAround (== '\"') v
    in return $ Token stripped





fetchRandomArticle :: Manager -> IO (Maybe Summary)
fetchRandomArticle manager = do
  request' <- parseRequest "https://en.wikipedia.org/api/rest_v1/page/random/summary"
  let request = request' { method = "GET", requestHeaders = [("User-Agent", "Haskell")]}
  response <- httpLbs request manager
  putStrLn $ show response
  return $ decode $ responseBody response

login :: Manager -> String -> String -> String -> IO (Maybe Token)
login manager endpoint username password = do
  initialRequest <- parseRequest endpoint
  let request = initialRequest { method = "POST"
                               , requestHeaders = [("Content-Type", "application/json")]
                               , requestBody = RequestBodyLBS $ encode $ object [ "username" .= username
                                                                                 , "password" .= password
                                                                                 ]
                               }
  response <- httpLbs request manager
  putStrLn $ show response
  return $ decode $ responseBody response

--addConcept :: Manager -> String -> Text -> Text -> Text -> Text -> IO ()
--addConcept manager endpoint authToken conceptName conceptDesc conceptLink = do
--  initialRequest <- parseRequest endpoint
--  let request = initialRequest { method = "POST"
--                               , requestHeaders = [ ("Content-Type", "application/json")
--                                                  , ("Authorization", encodeUtf8 $ "Bearer " <> authToken)
--                                                  ]
--                               , requestBody = RequestBodyLBS $ encode $ object [ "concept_name" .= conceptName
--                                                                                 , "concept_description" .= conceptDesc
--                                                                                 , "concept_wikilink" .= conceptLink
--                                                                                 ]
--                               }
--  response <- httpLbs request manager
--  putStrLn $ show response
--  putStrLn $ "Added concept: " ++ T.unpack conceptName
  
  

addConcept :: Manager -> String -> Text -> Text -> Text -> Text -> IO ()
addConcept manager endpoint authToken conceptName conceptDesc conceptLink = do
  let encodedConceptName = T.unpack $ decodeUtf8 $ urlEncode True $ encodeUtf8 conceptName
  let encodedConceptDesc = T.unpack $ decodeUtf8 $ urlEncode True $ encodeUtf8 conceptDesc
  let encodedConceptLink = T.unpack $ decodeUtf8 $ urlEncode True $ encodeUtf8 conceptLink

  initialRequest <- parseRequest $ endpoint ++ encodedConceptName ++ "/" ++ encodedConceptDesc ++ "/" ++ encodedConceptLink
  let request = initialRequest { method = "POST"
                               , requestHeaders = [ ("Content-Type", "application/json")
                                                  , ("Authorization", encodeUtf8 $ "Bearer " <> authToken)
                                                  ]
                               }
  response <- httpLbs request manager
  putStrLn $ show response
  putStrLn $ "Added concept: " ++ T.unpack conceptName


populateDB :: IO ()
populateDB = do
  manager <- newManager tlsManagerSettings

  maybeToken <- login manager "http://localhost:8080/login" "test" "test"
  case maybeToken of
    Just token' -> replicateM_ 2000 $ do
      maybeSummary <- fetchRandomArticle manager
      case maybeSummary of
        Just summary -> addConcept manager "http://localhost:8080/add-concept/" (token token') (title summary) (extract summary) (desktop $ content_urls summary)
        Nothing -> putStrLn "Failed to fetch article summary"
    Nothing -> putStrLn "Failed to login"












--fetchRandomArticle :: Manager -> IO (Maybe Summary)
--fetchRandomArticle manager = do
--  request' <- parseRequest "https://en.wikipedia.org/api/rest_v1/page/random/summary"
--  let request = request' { method = "GET", requestHeaders = [("User-Agent", "Haskell")]}
--  response <- httpLbs request manager
--  putStrLn $ show response
--  return $ decode $ responseBody response
--
--login :: Manager -> String -> String -> String -> IO (Maybe Token)
--login manager endpoint username password = do
--  initialRequest <- parseRequest endpoint
--  let request = initialRequest { method = "POST"
--                               , requestHeaders = [("Content-Type", "application/json")]
--                               , requestBody = RequestBodyLBS $ encode $ object [ "username" .= username
--                                                                                 , "password" .= password
--                                                                                 ]
--                               }
--  response <- httpLbs request manager
--  putStrLn $ show response
--  return $ decode $ responseBody response
--
--addConcept :: Manager -> String -> Text -> Text -> Text -> Text -> IO ()
--addConcept manager endpoint authToken conceptName conceptDesc conceptLink = do
--  initialRequest <- parseRequest endpoint
--  let request = initialRequest { method = "POST"
--                               , requestHeaders = [ ("Content-Type", "application/json")
--                                                  , ("Authorization", encodeUtf8 $ "Bearer " <> authToken)
--                                                  ]
--                               , requestBody = RequestBodyLBS $ encode $ object [ "concept_name" .= conceptName
--                                                                                 , "concept_description" .= conceptDesc
--                                                                                 , "concept_wikilink" .= conceptLink
--                                                                                 ]
--                               }
--  --response <- httpLbs request manager
--  response <- httpLbs request manager
--
--  putStrLn $ show response
--
--  putStrLn $ "Added concept: " ++ T.unpack conceptName
--  
--
--populateDB :: IO ()
--populateDB = do
--  manager <- newManager tlsManagerSettings
--
--  maybeToken <- login manager "http://localhost:8080/login" "test" "test"
--  case maybeToken of
--    Just token' -> replicateM_ 2000 $ do
--      maybeSummary <- fetchRandomArticle manager
--      case maybeSummary of
--        Just summary -> addConcept manager "http://localhost:8080/add-concept" (token token') (title summary) (description summary) (url $ desktop $ content_urls summary)
--        Nothing -> putStrLn "Failed to fetch article summary"
--    Nothing -> putStrLn "Failed to login" 
--
