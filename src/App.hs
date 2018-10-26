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
-}

handlerPutStepsFromStudent :: StepsWithKey -> Handler ResponseMsg 
handlerPutStepsFromStudent stepsWithKey = do
  dbUrl <- liftIO $ fmap (fromMaybe "") (lookupEnv "DATABASE_URL") -- IO String
  dbConnection <- liftIO $ connectPostgreSQL $ ByteString.pack dbUrl 
  -- TODO Fix
  let evSteps = evalSteps stepsWithKey
      studentKey = key stepsWithKey
  return $ ResponseMsg "Success"


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