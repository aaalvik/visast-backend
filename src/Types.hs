{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where 

import Data.Aeson
import GHC.Generics

-- * Types 

data GenericAST = GenericAST {
    name :: Name,  
    children :: [GenericAST]
} deriving (Eq, Show, Read, Generic)

instance ToJSON GenericAST   
instance FromJSON GenericAST 


data Steps = Steps { 
    evalSteps :: [GenericAST]
} deriving (Show, Generic)

instance ToJSON Steps
instance FromJSON Steps



type Name = String 


data InputString = Input {
    str :: String 
} deriving (Eq, Show, Generic)

instance FromJSON InputString 

newtype ResponseMsg = ResponseMsg { resStr :: String } deriving (Show, Generic)
instance ToJSON ResponseMsg
