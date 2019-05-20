module Eval where
    
    import Syntax

    bopeval :: BinOp -> Expr -> Expr -> Expr
    bopeval Add (Int x) (Int y) = (Int (x+y))
    bopeval Sub (Int x) (Int y) = (Int (x-y))
    bopeval Mul (Int x) (Int y) = (Int (x*y))
    bopeval Eql (Int x) (Int y) = (Bool (x==y))
    bopeval Eql (Bool x) (Bool y) = (Bool (x==y))

    eval :: Expr -> Env -> Expr
    eval (Var x) xs = let vals = filter (\n -> x == fst n) xs in
                        case vals of
                            [] -> error "Variable not found in environment"
                            [(_, v)] -> v
                            (v:vs) -> error ("More than one value for "++x++" in environment")
    eval (Int x) xs = Int x
    eval (Bool x) xs = Bool x
    eval (Let x e1 e2) xs = eval e2 ((x,(eval e1 xs)):xs)
    eval (Fn x e) xs = (Closure x e xs)
    eval (App fn e) xs = let Closure x e' xs' = eval fn xs in
                            eval e' ((x, (eval e xs)):xs')
    eval (If e1 e2 e3) xs = let p = eval e1 xs in
                            if p==(Bool True) then (eval e2 xs) else (eval e3 xs)
    eval (Op b e1 e2) xs = eval (bopeval b (eval e1 xs) (eval e2 xs)) xs
