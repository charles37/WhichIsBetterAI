{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- turn off waringings for type-defaults




module Tagger.AI where

import OpenAI.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
--import System.Environment (getEnv)
import qualified Data.Text as T



import Data.Text.Metrics



-- the Jaro-Winkler distance, which is a measure of similarity between two strings. 
-- It is a variant of the Jaro distance metric and mainly used in the area of record linkage (duplicate detection). 
-- The higher the Jaro-Winkler distance for two strings is, the more similar the strings are. The Jaro-Winkler distance metric is designed and best suited for short strings such as person names.
similarEnough :: T.Text -> T.Text -> Bool
similarEnough s1 s2 =
  let similarity = jaroWinkler (T.toLower s1) (T.toLower s2) 
  in similarity >= 0.85





--import Configuration.Dotenv (loadFile, defaultConfig)

-- | runComparison :: ModelName -> ConceptName -> ConceptName -> IO ConceptName
-- | takes a model name, and two concept names, and returns the name of the winning concept from that model
-- | in the future we will have different models but for now just always use the openai models
-- | Also in the future the error system here will be better
runComparison :: T.Text -> T.Text -> T.Text -> IO T.Text
runComparison _ concept1 concept2 =
  do manager <- newManager tlsManagerSettings
     --_ <- loadFile defaultConfig
     --apiKey <- T.pack <$> getEnv "OPENAI_KEY"
     let apiKey = T.pack "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" -- VERY UNSAFE BUT REPO IS PRIVATE

     -- create a openai client that automatically retries up to 4 times on network
     -- errors
     let client = makeOpenAIClient apiKey manager 4

     -- Long drawn out prompt really exclaiming that the ai should only output one of the two choices and only that
     --let promptText choice1 choice2 = "Which is better, " <> choice1 <> " or " <> choice2 <> "? " <> "You must output only one of the choice and no other words"

     let request = ChatCompletionRequest 
             { chcrModel = ModelId "gpt-3.5-turbo"
             , chcrMessages = 
             -- its imporant that the prompt is engineered so that it will alway just output one of the two choices and nothing else
                [ChatMessage { chmContent = Just $ "Which is better, " <> concept1 <> " or " <> concept2 <> "? " <> "You must output only one of the choices and no other words"
                               , chmRole = "user"
                               , chmFunctionCall = Nothing
                               , chmName = Nothing
                             }
                ]
             , chcrTemperature = Nothing
             , chcrTopP = Nothing
             , chcrN = Nothing
             , chcrStream = Nothing
             , chcrStop = Nothing
             , chcrMaxTokens = Nothing
             , chcrPresencePenalty = Nothing
             , chcrFrequencyPenalty = Nothing
             , chcrLogitBias = Nothing
             , chcrUser = Nothing
             , chcrFunctions = Nothing
             }
     result <- completeChat client request        
     case result of
       Left failure -> return $ T.pack $ show failure
       Right success -> case chmContent $ chchMessage $ head $ chrChoices success of
        Just c ->
            case (similarEnough c concept1, similarEnough c concept2) of
                (True, False) -> return concept1
                (False, True) -> return concept2
                (True, True) -> return $ T.pack $ show $ head $ chrChoices success
                (False, False) -> return $ T.pack $ show $ head $ chrChoices success
        Nothing -> return $ T.pack $ show $ head $ chrChoices success
           



 








