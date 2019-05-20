-- Type checker will check the statement for type safety if it is valid will return back the type of the expression,
-- otherwise will return a type error type
module TypeCheck where
    import Syntax

    data TypeError
            = Unexpected Type Type
            | NotInScope Name
            | NotAFunction Type
            deriving (Show, Eq)

    type TypeContext = [(Name, Type)]

    lookupVar :: Name -> TypeContext -> Either TypeError Type
    lookupVar n xs = case lookup n xs of
                        Just t  -> Right t
                        Nothing -> Left (NotInScope n)

    bopType :: BinOp -> Type
    bopType Eql = FnT (FnT IntT IntT) IntT
                        

    check :: Expr -> TypeContext -> Either TypeError Type
    check (Int x) _         = Right IntT
    check (Bool x) _        = Right BoolT
    check (Var x) xs        = lookupVar x xs
    check (Let n e1 e2) xs  = let t1 = check e1 xs in
                                case t1 of
                                    Left err -> Left err
                                    Right ty -> check e2 ((n,ty):xs)
    check (Fn n t1 e1) xs   = let t2 = check e1 ((n,t1):xs) in
                                case t2 of
                                    Left err -> Left err
                                    Right ty -> Right (FnT t1 ty)
    check (App e1 e2) xs    = let t1 = check e1 xs
                                  t2 = check e2 xs
                                in
                                case (t1, t2) of
                                    (_, Left err) -> Left err
                                    (Left err, _) -> Left err
                                    (Right (FnT a b), Right ty) -> if a == ty then Right b else Left (Unexpected a ty)
                                    (Right ty, _) -> Left (NotAFunction ty)
    check (If e1 e2 e3) xs  = let t1 = check e1 xs
                                  t2 = check e2 xs
                                  t3 = check e3 xs
                                in
                                case (t1, t2, t3) of
                                    (Left err, _, _) -> Left err
                                    (_, Left err, _) -> Left err
                                    (_, _, Left err) -> Left err
                                    (Right BoolT, Right ty1, Right ty2) | ty1 == ty2 -> Right ty1
                                                                        | otherwise -> Left (Unexpected ty1 ty2)
                                    (Right ty, _, _) -> Left (Unexpected BoolT ty)                               
    check (Op b e1 e2) xs   = let t1 = check e1 xs
                                  t2 = check e2 xs
                                in
                                case (b, t1, t2) of
                                    (Eql, Right IntT, Right IntT) -> Right BoolT
                                    (Eql, Right BoolT, Right BoolT) -> Right BoolT
                                    (Eql, Right ty1, Right ty2) -> Left (Unexpected ty1 ty2)
                                    (_, Right IntT, Right IntT) -> Right IntT
                                    (_, _, _) -> Left (Unexpected IntT UnknownT)
                                                                       
    test = Fn "x" IntT (If (Op Eql (Var "x") (Int 5)) (Bool True) (Bool False)) 
    test2 = Let "f" (Fn "x" IntT (Op Add (Var "x") (Int 5))) (App (Var "f") (Bool True))
    test3 = Fn "x" IntT (Fn "y" IntT (Op Add (Var "x") (Var "y")))
