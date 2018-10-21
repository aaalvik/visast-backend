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
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import System.Environment
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options

-- * API

type GenericASTAPI =
  "steps" :> ReqBody '[JSON] InputString :> Post '[JSON] [GenericAST]


-- * APP

run :: IO ()
run = do
  port <- read <$> getEnv "PORT"
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp


mkApp :: IO Application
mkApp = 
  return $ 
  cors (const $ Just policy) $
  provideOptions (Proxy :: Proxy GenericASTAPI) $
  serve (Proxy :: Proxy GenericASTAPI) server
  where 
    policy = simpleCorsResourcePolicy 
              { corsRequestHeaders = [ "content-type" ] 
              , corsMethods = [ "GET", "POST"]
              }


handlerSteps :: InputString -> Handler [GenericAST]
handlerSteps inputStr = do
  let s = str inputStr 
      startExpr = Parser.parse s
      steps = Evaluator.eval startExpr 
  return $ map Convert.toGeneric steps
    

server :: Server GenericASTAPI
server = handlerSteps