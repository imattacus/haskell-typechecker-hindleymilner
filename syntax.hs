module Syntax where

    type Name = String
    type Env = [(Name, Expr)]

    data Expr 
            = Var Name
            | Int Integer
            | Bool Bool
            | Closure Name Expr Env
            | Let Name Expr Expr
            | Fn Name Expr
            | App Expr Expr
            | If Expr Expr Expr
            | Op BinOp Expr Expr
            deriving (Show, Eq, Ord)
    
    data BinOp
            = Add
            | Sub
            | Mul
            | Eql
            deriving (Show, Eq, Ord)
        
    newtype TVar = TV String deriving (Show, Eq, Ord)
    
    data Type
            = VarT TVar
            | IntT
            | BoolT
            | FnT Type Type
            deriving (Show, Eq, Ord)
