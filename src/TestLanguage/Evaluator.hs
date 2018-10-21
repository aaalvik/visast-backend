module TestLanguage.Evaluator where 

import TestLanguage.TestTypes


-- Evaluator for visualization. Returns a list of all evaluation steps 
eval :: Expr -> [Expr]
eval (Num num) = [Num num] 
eval expr = 
    expr : eval (evalSmallStep expr)


-- Big-step evaluator 
evalBigStep :: Expr -> Int 
evalBigStep expr = 
    case expr of 
        Num num -> num 
        _ -> evalBigStep $ evalSmallStep expr 


-- Small-step evaluator 
evalSmallStep :: Expr -> Expr 
evalSmallStep (Num num) = Num num 

evalSmallStep (Add (Num n1) (Num n2)) = Num $ n1 + n2 
evalSmallStep (Add (Num n1) e2) = Add (Num n1) (evalSmallStep e2)
evalSmallStep (Add e1 e2) = Add (evalSmallStep e1) e2

evalSmallStep (Mult (Num n1) (Num n2)) = Num $ n1 * n2 
evalSmallStep (Mult (Num n1) e2) = Mult (Num n1) (evalSmallStep e2)
evalSmallStep (Mult e1 e2) = Mult (evalSmallStep e1) e2

evalSmallStep (Neg (Num num)) = Num $ num * (-1)
evalSmallStep (Neg e) = Neg $ evalSmallStep e 

evalSmallStep (If (Num num) eThen eElse) = if num /= 0 then eThen else eElse
evalSmallStep (If cond eThen eElse) = If (evalSmallStep cond) eThen eElse 