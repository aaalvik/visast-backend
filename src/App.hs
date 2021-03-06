{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where


import Types 
import TestLanguage.Evaluator as Evaluator
import TestLanguage.Parser as Parser 
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as ByteString
import Database.PostgreSQL.Simple
import GHC.Int
import GenericAST
import Control.Monad

-- * API

type API = "easy" :> ReqBody '[JSON] InputString :> Post '[JSON] [GenericAST]
      :<|> "advanced" :> Capture "lookupKey" String :> ReqBody '[JSON] Steps :> Put '[JSON] ResponseMsg
      :<|> "advanced" :> QueryParam "lookupKey" String :> Get '[JSON] [GenericAST]
  

-- * APP

run :: IO ()
run = do
  port <- fmap (fromMaybe "3000") (lookupEnv "PORT")
  let 
    settings =
        setPort (read port) $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ port)) $
        defaultSettings
  runSettings settings =<< mkApp


mkApp :: IO Application
mkApp = 
  return $ 
  cors (const $ Just policy) $
  provideOptions (Proxy :: Proxy API) $
  serve (Proxy :: Proxy API) server
  where 
    policy = simpleCorsResourcePolicy 
              { corsRequestHeaders = [ "content-type", "Access-Control-Allow-Origin" ] 
              , corsMethods = [ "GET", "POST", "PUT"]
              }


handlerEasy :: InputString -> Handler [GenericAST]
handlerEasy inputStr = do
  -- Logging 
  liftIO $ hPutStrLn stderr "Requested steps in easy mode"
  let s = str inputStr 
      mStartExpr = Parser.parse s
      steps = case mStartExpr of 
        Just expr -> 
          Evaluator.eval expr
        Nothing -> []
  return $ map toGeneric steps


queryDoesKeyExist :: Connection -> String -> IO Bool 
queryDoesKeyExist conn key = do 
  let queryStr = "SELECT COUNT(*) FROM StudentSteps WHERE Key = ?"

  [Only num] <- query conn queryStr (Only key) :: IO [Only Int64]
  if num == 0 then 
    return False 
  else return True 
  

queryPutSteps :: Connection -> String -> [GenericAST] -> IO GHC.Int.Int64
queryPutSteps conn key steps = do 
  -- Check if key exists already
  keyExists <- queryDoesKeyExist conn key 

  let queryStr = if keyExists 
        then "UPDATE StudentSteps SET Steps = ? WHERE Key = ?;"
        else "INSERT INTO StudentSteps (Key, Steps) VALUES (?, ?);"

  if keyExists 
    then execute conn queryStr (show steps :: String, key :: String)
    else execute conn queryStr (key :: String, show steps :: String)


handlerAdvancedPut :: String -> Steps -> Handler ResponseMsg 
handlerAdvancedPut lookupKey steps = do
  let evSteps = evalSteps steps
  -- Logging 
  liftIO $ hPutStrLn stderr $ "User (" ++ lookupKey ++ ") called visualise"

  dbUrl <- liftIO $ fmap (fromMaybe "") (lookupEnv "DATABASE_URL")
  dbConnection <- liftIO $ connectPostgreSQL $ ByteString.pack dbUrl 

  rowsAffected <- liftIO $ queryPutSteps dbConnection lookupKey evSteps
  liftIO $ close dbConnection

  if rowsAffected == 1 then
    return $ ResponseMsg "Success"
  else return $ ResponseMsg "Failure"


queryGetSteps :: Connection -> String -> IO [GenericAST]
queryGetSteps conn key = do 
  let queryStr = "SELECT Steps FROM StudentSteps WHERE Key = ?"
  xs <- query conn queryStr (Only key) :: IO [Only String]
  stepsList <- forM xs (return . (read :: String -> [GenericAST]) . fromOnly )

  if null stepsList then 
    return [] 
  else do 
    return $ head stepsList 
    -- I only care about first entry, should only be one 


handlerAdvancedGet :: Maybe String -> Handler [GenericAST]
handlerAdvancedGet mKey =
  case mKey of 
    Nothing -> return []
    Just lookupKey -> do 
      -- Logging 
      liftIO $ hPutStrLn stderr $ "User (" ++ lookupKey ++ ") requested saved steps"
      

      dbUrl <- liftIO $ fmap (fromMaybe "") (lookupEnv "DATABASE_URL")
      dbConnection <- liftIO $ connectPostgreSQL $ ByteString.pack dbUrl 

      steps <- liftIO $ queryGetSteps dbConnection lookupKey 
      return steps


server :: Server API
server = handlerEasy :<|> handlerAdvancedPut :<|> handlerAdvancedGet 