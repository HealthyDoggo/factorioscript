module AST where

import Data.Text (Text) -- use Text instead of String as more efficient

type Name = Text
type SignalName = Text

data Direction = Input | Output
    deriving (Show, Eq)

data TypeAnn = AnnInt | AnnBool -- prefix to avoid clash with builtin, Ann not T to avoid inference clashes.
    deriving (Show, Eq)

data Binop -- applies to any exprs, so covers all basic operators; both boolean and arithmetic.
    = Add | Sub | Mul | Div | Exp | Mod -- arith
    | Equal | NotEqual | Less | Greater | LessEq | GreaterEq -- arith comparison
    | And | Or -- bool
    deriving (Show, Eq, Ord)

data Unop -- unary operator, so operates on one value
    = Not | Neg
    deriving (Show, Eq, Ord)

data IODecl = IODecl -- e.g. input "signal-A" input_a Int "Put base here"
    { ioDirection :: Direction
    , ioSignal    :: SignalName
    , ioName      :: Name
    , ioType      :: TypeAnn
    , ioLabel     :: Maybe Text
    }
    deriving (Show, Eq)

data Lit = LInt Integer | LBool Bool
    deriving (Show, Eq, Ord)

data Pragma = TailAssert
    deriving (Show, Eq, Ord)

data Expr
    = Var Name
    | Lit Lit
    | App Expr Expr
    | BinOp Binop Expr Expr
    | UnOp Unop Expr
    | If Expr Expr Expr
    | Let Bool [Def] Expr -- list of defs to allow for let ... and ... and ... in 
    | Pragma Pragma Expr
    deriving (Show, Eq, Ord)

data Def = Def -- function definition
    { defName   :: Name
    , defParams :: [Name]
    , defBody   :: Expr
    } deriving (Show, Eq, Ord)

data Binding = Binding { bindRec :: Bool, bindDefs :: [Def] }
    deriving (Show, Eq, Ord)

data TopItem = TopDecl IODecl | TopBinding Binding -- Lists can only contain one type, so use an additive type for decls and bindings.
    deriving (Show, Eq)

type Program = [TopItem]

-- helpers

-- OpClass is here to stay singleton in case more ops are added.
data OpClass = Arith | Compare | BLogic -- 3 types of operators: arithmetic; comparison; boolean logic

opClass :: Binop -> OpClass -- function to return type of operator. Scuffed but readable and easily updated.
opClass Add         = Arith
opClass Sub         = Arith
opClass Mul         = Arith
opClass Div         = Arith
opClass Exp         = Arith
opClass Mod         = Arith
opClass Equal       = Compare
opClass NotEqual    = Compare
opClass Greater     = Compare
opClass Less        = Compare
opClass GreaterEq   = Compare
opClass LessEq      = Compare
opClass And         = BLogic
opClass Or          = BLogic