module TestLanguage.Convert where 

import TestLanguage.TestTypes 
import Types 

toGeneric :: Expr -> GenericAST 
toGeneric n@(Num _) = GenericAST (show n) []
toGeneric (Add e1 e2) = GenericAST "Add" [toGeneric e1, toGeneric e2] 
toGeneric (Mult e1 e2) = GenericAST "Mult" [toGeneric e1, toGeneric e2]
toGeneric (Neg e) = GenericAST "Neg" [toGeneric e]
toGeneric (If cond eThen eElse) = 
    GenericAST "If" [toGeneric cond, toGeneric eThen, toGeneric eElse]