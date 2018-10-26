{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where


import Types 
import TestLanguage.Evaluator as Evaluator
import TestLanguage.Parser as Parser 
import TestLanguage.Convert as Convert 
import TestLanguage.TestTypes as TestTypes (Expr)
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
import qualified Data.Text as Text
import Control.Monad

-- * API

type API = "steps" :> ReqBody '[JSON] InputString :> Post '[JSON] [GenericAST]
      :<|> "putStepsFromStudent" :> ReqBody '[JSON] StepsWithKey :> Post '[JSON] ResponseMsg
      :<|> "getStepsFromStudent" :> QueryParam "studentKey" String :> Get '[JSON] [GenericAST]
  

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
              , corsMethods = [ "GET", "POST"]
              }


handlerSteps :: InputString -> Handler [GenericAST]
handlerSteps inputStr = do
  let s = str inputStr 
      mStartExpr = Parser.parse s
      steps = case mStartExpr of 
        Just expr -> 
          Evaluator.eval expr
        Nothing -> []
  return $ map Convert.toGeneric steps

{- Tabell: StudentSteps ( Key varchar(255), Steps TEXT ); -}


queryInsertSteps :: Connection -> String -> [GenericAST] -> IO GHC.Int.Int64
queryInsertSteps conn key steps = do 
  let queryStr = "INSERT INTO StudentSteps (Key, Steps) VALUES (?, ?);"
    -- "INSERT INTO StudentSteps (Key, Steps) VALUES (?, ?) ON CONFLICT (Key) DO UPDATE SET Steps = ?;"
  execute conn queryStr (key :: String, show steps :: String)


handlerPutStepsFromStudent :: StepsWithKey -> Handler ResponseMsg 
handlerPutStepsFromStudent stepsWithKey = do
  let evSteps = evalSteps stepsWithKey
      studentKey = key stepsWithKey
  dbUrl <- liftIO $ fmap (fromMaybe "") (lookupEnv "DATABASE_URL")
  dbConnection <- liftIO $ connectPostgreSQL $ ByteString.pack dbUrl 

  rowsAffected <- liftIO $ queryInsertSteps dbConnection studentKey evSteps
  liftIO $ close dbConnection

  if rowsAffected == 1 then 
    return $ ResponseMsg "Success"
  else return $ ResponseMsg dbUrl --"Failure"


queryGetSteps :: Connection -> String -> IO [GenericAST]
queryGetSteps conn key = do 
  let queryStr = "SELECT Steps FROM StudentSteps WHERE Key = ?"
  xs <- query conn queryStr [key]
  stepsList <- forM xs (\str -> return $ (read str :: [GenericAST]))

  if null stepsList then 
    return [] 
  else do 
    return $ head stepsList 
    -- I only case about first entry, should only be one 


handlerGetStepsFromStudent :: Maybe String -> Handler [GenericAST]
handlerGetStepsFromStudent mKey = 
  case mKey of 
    Nothing -> return []
    Just sKey -> do 
      dbUrl <- liftIO $ fmap (fromMaybe "") (lookupEnv "DATABASE_URL")
      dbConnection <- liftIO $ connectPostgreSQL $ ByteString.pack dbUrl 

      steps <- liftIO $ queryGetSteps dbConnection sKey 
      return steps


server :: Server API
server = handlerSteps :<|> handlerPutStepsFromStudent :<|> handlerGetStepsFromStudent 