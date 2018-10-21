module TestLanguage.TestMain where 

import TestLanguage.Evaluator 
import TestLanguage.Parser 

testMain = do 
    input <- getLine 
    print $ eval $ parse input 