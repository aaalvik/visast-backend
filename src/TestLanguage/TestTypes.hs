{-# LANGUAGE DeriveGeneric #-}

module TestLanguage.TestTypes where 

import GHC.Generics
import GenericAST

data Expr 
    = Num Int 
    | Add Expr Expr 
    | Mult Expr Expr 
    | Neg Expr 
    | If Expr Expr Expr 
    deriving (Eq, Show, Generic)

instance Generalise Expr 