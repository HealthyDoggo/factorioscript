module Types where

import AST -- imported for TypeAnns
import qualified Data.Text as T -- imported for text manipulation in prettyPrint
import Data.Map

newtype TyVar = TV T.Text -- unsolved type e.g. type a; could be anything
    deriving (Show, Eq, Ord)

data Type
    = TVar TyVar -- unsolved type variable
    | TInt -- integer
    | TBool -- boolean
    | TArr Type Type -- TArr = type arrow. reads as Type -> Type for functions
    deriving (Show, Eq)

{- Reads as or any [a, b], expression is type a -> b for example. Key to polymorphism. 
For non-generalised expressions, the list of general variables will be empty. -}
data Scheme = Forall [TyVar] Type
    deriving (Show, Eq)

type Subst = Map TyVar Type -- map of solved type variables to types
newtype TypeEnv = TypeEnv (Map Name Scheme) -- map of variables to their type expressions

{- For now, our TypeErrors (TEs) are stored simply like this. In the future, if a system is built to 
locate the origins of TEs, then perhaps this will be changed to a record with context and kind -}
data TypeError
    = Mismatch Type Type -- variable is solved to 2 different types
    | InfiniteType TyVar Type -- e.g. omega combinator - f x = x x - cannot be resolved in HM
    | UnboundVariable Name -- self-explanatory.
    deriving (Show, Eq)

annToType :: TypeAnn -> Type -- converts type annotations from i/o to type literals
annToType AnnInt  = TInt
annToType AnnBool = TBool

prettyPrint :: Type -> T.Text -- primarily for debugging and error msgs, pretty print types
prettyPrint TInt                    = "Int"
prettyPrint TBool                   = "Bool"
prettyPrint (TVar (TV t))           = t
prettyPrint (TArr arg@(TArr _ _) y) = T.concat ["(", prettyPrint arg, ")", " -> ", prettyPrint y] 
-- above to show associativity when functions passed as arguments, e.g. (a -> b) -> c -> d
prettyPrint (TArr x y)              = T.concat [prettyPrint x, " -> ", prettyPrint y] -- e.g. a -> b
