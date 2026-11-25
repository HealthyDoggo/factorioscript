module Infer where

import Types
-- TODO: import Subst
import AST
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

type Infer = ExceptT TypeError (State Int) -- Int is carried in a state so it can be incremented to stay unique
{- The above Monad describes a computation that either fails, returning Left (a TypeError) or returns the passed type.
Therefore, Infer Type describes a computation that either throws a TypeError or returns a Type. 
State Int is here as the original monad. If the operation couldn't fail, it would be State Int Type.
Now, instead, it is something roughly like State Int (Either TypeError Type). -}

-- function for running type logic in the Infer monad
runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState {- runs within state -} (runExceptT m) {- run within ExceptT -} 0 {- start counter at 0 -} of
    Left err -> Left err
    Right res -> Right $ generalise res emptyEnv -- generalise resolved type
--------------------------------------------
-- helpers for inference
-------------------------------------------
fresh :: Infer Type -- new type variable creation
fresh = do
    n <- get -- get counter from state
    put (n + 1) -- add 1 to counter and put on state
    return (TVar (TV ("t" <> pack (show n)))) -- basically just returns "t{counter}." Building text from int is annoying.

instantiate :: Scheme -> Infer Type -- create new set of TVs for calling of generalised statement
instantiate (Forall vs t) = do
    tvs <- mapM (const fresh) vs
    {- above line takes some explaining. mapM applies a monadic expression to each element of an iterable; 
    const fresh curries to return a function that always returns fresh as const ignores second arg.
    Essentially, it creates the scheme with fresh variables where the existing names were. This is used for polymorphism,
    so that each use of a variable gets a new type variable which allows its type to change on each use (polymorphism). -}
    let s = fromList (zip vs tvs) -- map from variables to fresh type variables
    return (apply s t) -- applies the substitution of var:tvar to the scheme. 

generalise :: TypeEnv -> Type -> Scheme -- generalise a type scheme, making it forall
generalise e t = Forall vs t
    where vs = Set.toList $ ftv t `Set.difference` ftv e
{- This one too. ftv = free type variables. vs is the difference of the overall environment and the current type of 
FTVs. This is this way to ensure that if something is elsewhere bound it's not generalised over in the current expr. 
At the time of calling, that expression is committed to a type. It is not the caller's choice what that is. -}

binopType :: Binop -> (Type, Type, Type) -- 2 args & result
binopType op = case opClass op of -- lists singletoned in AST so that new ops can be added in 1 place (hopefully).
    Arith   -> (TInt, TInt, TInt) -- Int op Int -> Int
    Compare -> (TInt, TInt, TBool) -- Int op Int -> Bool
    BLogic   -> (TBool, TBool, TBool) -- Bool op Bool -> Bool

--------------------------------------------
-- inference functions
--------------------------------------------

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer e x = case x of

    Var n               -> lookupEnv n a -- look up variable in environment
    Lit (LInt _)        -> TInt
    Lit (LBool _)       -> TBool
    App f a             -> undefined -- TODO
    BinOp op l r        -> undefined -- TODO
    UnOp op arg         -> undefined -- TODO
    If cond tr fl       -> undefined -- TODO
    Let binding body    -> undefined -- TODO
    Pragma _ expr       -> infer expr   

inferDef :: TypeEnv -> Def -> Infer (Subst, TypeEnv)
inferDef = undefined -- TODO

inferGroup :: TypeEnv -> Binding -> Infer (Subst, TypeEnv)
inferGroup = undefined -- TODO

--------------------------------------------
-- more
--------------------------------------------

inferProgram :: Program -> Either TypeError TypeEnv -- essentially the main function of inference
inferProgram = undefined -- TODO


