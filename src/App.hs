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

{-
To run IO inside Handlers, just use 'liftIO': 
  filecontent <- liftIO (readFile "myfile.txt")

Run queries (SELECT): 
query conn "select ? + ?" (40,2)

Run INSERTs:
execute :: ToRow q => Connection -> Query -> q -> IO Int64
-}

insertSteps :: Connection -> String -> [GenericAST] -> IO GHC.Int.Int64
insertSteps conn key steps = do 
  let queryStr = "INSERT INTO StudentSteps (Key, Steps) VALUES (?, ?) ON CONFLICT (Key) DO UPDATE SET Steps = ?;"
  execute conn queryStr (key :: String, show steps :: String, show steps :: String)

{-
Tabell: StudentSteps ( Key varchar(255), Steps TEXT );

INSERT INTO the_table (id, column_1, column_2) 
VALUES (1, 'A', 'X'), (2, 'B', 'Y'), (3, 'C', 'Z')
ON CONFLICT (id) DO UPDATE 
  SET column_1 = excluded.column_1, 
      column_2 = excluded.column_2;
-}


handlerPutStepsFromStudent :: StepsWithKey -> Handler ResponseMsg 
handlerPutStepsFromStudent stepsWithKey = do
  let evSteps = evalSteps stepsWithKey
      studentKey = key stepsWithKey
  dbUrl <- liftIO $ fmap (fromMaybe "") (lookupEnv "DATABASE_URL") -- IO String
  dbConnection <- liftIO $ connectPostgreSQL $ ByteString.pack dbUrl 

  rowsAffected <- liftIO $ insertSteps dbConnection studentKey evSteps
  liftIO $ close dbConnection

  if rowsAffected == 1 then 
    return $ ResponseMsg "Success"
  else return $ ResponseMsg "Failure"


handlerGetStepsFromStudent :: Maybe String -> Handler [GenericAST]
handlerGetStepsFromStudent mKey = 
  case mKey of 
    Nothing -> return []
    Just sKey ->
      if sKey == "raa009" then 
        return [ GenericAST "Num 1" [] ]
      else return [] -- error
      -- TODO lookup in map 


server :: Server API
server = handlerSteps :<|> handlerPutStepsFromStudent :<|> handlerGetStepsFromStudent 