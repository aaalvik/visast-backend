module TestLanguage.Parser where
    
import Data.Char
import TestLanguage.TestTypes

{- Grammatikk for språket Expr :
S -> + S S | * S S | - S | if S then S else S | A 
A -> number
-}


parse :: String -> Maybe Expr 
parse str = 
    let 
        tokens = tokenize str 
    in 
        case parseExpr tokens of 
            (Just expr, []) -> 
                Just expr 
            _ -> Nothing -- parsing failed somehow


tokenize :: String -> [String]
tokenize [] = [] 
tokenize (' ':xs) = tokenize xs 
tokenize s@(x:xs) 
    | elem x "+*-" = [x] : tokenize xs 
    | isDigit x = (takeWhile isDigit s) : tokenize (dropWhile isDigit s)
tokenize ('i':'f':xs) = "if" : tokenize xs 
tokenize ('t':'h':'e':'n':xs) = "then" : tokenize xs 
tokenize ('e':'l':'s':'e':xs) = "else" : tokenize xs 


parseExpr :: [String] -> (Maybe Expr, [String])
parseExpr (x:xs) | all isDigit x = 
    (Just $ Num $ read x, xs)

parseExpr ("+":xs) =
    let 
        (me1, rest) = parseExpr xs 
        (me2, rest') = parseExpr rest 
    in 
        case (me1, me2) of 
            (Just e1, Just e2) -> 
                (Just $ Add e1 e2, rest')
            _ -> (Nothing, [])

parseExpr ("*":xs) =
    let 
        (me1, rest) = parseExpr xs 
        (me2, rest') = parseExpr rest 
    in 
        case (me1, me2) of 
            (Just e1, Just e2) -> 
                (Just $ Mult e1 e2, rest')
            _ -> (Nothing, [])

parseExpr ("-":xs) = 
    let 
        (me, rest) = parseExpr xs 
    in 
        (fmap Neg me, rest)

parseExpr ("if":xs) = 
    let 
        (meCond, restCond) = parseExpr xs                -- rest should start with "then"
        (meThen, restThen) = parseExpr $ tail restCond   -- rest should start with "else"
        (meElse, restElse) = parseExpr $ tail restThen
    in 
        case (meCond, meThen, meElse) of 
            (Just eCond, Just eThen, Just eElse) ->
                if head restCond == "then" && head restThen == "else" then 
                    (Just $ If eCond eThen eElse, restElse)
                else (Nothing, [])
            _ -> (Nothing, [])

parseExpr _ = (Nothing, [])
