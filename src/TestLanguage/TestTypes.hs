module TestLanguage.TestTypes where 

data Expr 
    = Num Int 
    | Add Expr Expr 
    | Mult Expr Expr 
    | Neg Expr 
    | If Expr Expr Expr 
    -- | Let Char Expr Expr
    -- | Var Char deriving
    deriving (Eq, Show)