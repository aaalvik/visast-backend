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

-- * API

type API = "steps" :> ReqBody '[JSON] InputString :> Post '[JSON] [GenericAST]
      :<|> "stepsFromStudent" :> ReqBody '[JSON] StepsWithKey :> Post '[JSON] ResponseMsg
  

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
      startExpr = Parser.parse s
      steps = Evaluator.eval startExpr 
  return $ map Convert.toGeneric steps


handlerStepsFromStudent :: StepsWithKey -> Handler ResponseMsg 
handlerStepsFromStudent stepsWithKey = do
  let evSteps = evalSteps stepsWithKey
      studentKey = key stepsWithKey
  -- TODO save in map of <key, evSteps> 
  return $ ResponseMsg { resStr = "Success"}


server :: Server API
server = handlerSteps :<|> handlerStepsFromStudent 