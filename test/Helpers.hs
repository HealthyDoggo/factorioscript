module Helpers where
import AST

int :: Integer -> Expr
int = Lit . LInt 

add, sub, mul, div, exp, mod :: Expr -> Expr -> Expr
add = BinOp Add
sub = BinOp Sub
mul = BinOp Mul
div = BinOp Div
exp = BinOp Exp
mod = BinOp Mod

app :: Expr -> [Expr] -> Expr
app = foldl App -- folds out the arguments for proper currying.
{- i.e. app (Var "add3") [int 1, int 2, int 3] goes to:
App (App (App "add3" (int 1)) (int 2)) (int 3) 
This is due to the nature of currying where if a function takes e.g two arguments, 
the function passed with 1 produces a new function that only takes 1 argument. -}
