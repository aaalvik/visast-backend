module TestLanguage.Parser where
    
import Data.Char
import TestLanguage.TestTypes


parse :: String -> Expr 
parse str = 
    let 
        tokens = tokenize str 
        (expr, rest) = parseExpr tokens
    in if null rest then expr 
    else error $ "Parsing didn't finish, stopped at: " ++ unwords rest 


tokenize :: String -> [String]
tokenize str = words str 
-- TODO: Fiks denne for å slippe mellomrom mellom alt 


parseExpr :: [String] -> (Expr, [String])
parseExpr (x:xs) | all isDigit x = 
    (Num $ read x, xs)

parseExpr ("+":xs) =
    let 
        (e1, rest) = parseExpr xs 
        (e2, rest') = parseExpr rest 
    in (Add e1 e2, rest')

parseExpr ("*":xs) =
    let 
        (e1, rest) = parseExpr xs 
        (e2, rest') = parseExpr rest 
    in (Mult e1 e2, rest')

parseExpr ("-":xs) = 
    let 
        (e, rest) = parseExpr xs 
    in (Neg e, rest)

parseExpr ("if":xs) = 
    let 
        (eCond, restCond) = parseExpr xs                -- rest should start with "then"
        (eThen, restThen) = parseExpr $ tail restCond   -- rest should start with "else"
        (eElse, restElse) = parseExpr $ tail restThen
    in 
    case (head restCond, head restThen) of 
        ("then", "else") -> 
            (If eCond eThen eElse, restElse)
        _ -> 
            error "parseExpr error: if-expression lacks then/else keyword"

parseExpr e = error $ "parseExpr error: expression doesn't match any rules" ++ show e
