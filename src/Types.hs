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
} deriving (Eq, Show, Generic)


instance ToJSON GenericAST  
instance FromJSON GenericAST 

type Name = String 


data InputString = Input {
    str :: String 
} deriving (Eq, Show, Generic)

instance FromJSON InputString 