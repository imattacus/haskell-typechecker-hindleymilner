-- Based on information from http://dev.stephendiehl.com/fun/006_hindley_milner.html
-- and Heeren et Al. "Generalising Hindley Milner Type Inference Algorithms"
-- and https://en.wikipedia.org/wiki/Hindley–Milner_type_system

module TypeInference where
    import Syntax

    import Control.Monad.State
    import Control.Monad.Except

    import Data.List (nub)
    import qualified Data.Map as Map
    import qualified Data.Set as Set

    import Control.Monad.Reader

    data TypeError
            = UnificationFail Type Type
            | InfiniteType TVar Type
            | UnboundVariable String
            deriving (Show, Eq)

    data Scheme = Forall [TVar] Type
        deriving (Show, Eq, Ord)

    newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

    data Unique = Unique { count :: Int }
        deriving (Show)

    type Infer = ExceptT TypeError (State Unique) 
    type Subst = Map.Map TVar Type

    nullSubst :: Subst
    nullSubst = Map.empty

    getOpT :: BinOp -> Type
    getOpT Add = (FnT IntT (FnT IntT IntT))
    getOpT Sub = (FnT IntT (FnT IntT IntT))
    getOpT Mul = (FnT IntT (FnT IntT IntT))
    getOpT Eql = (FnT IntT (FnT IntT BoolT))

    -- Extend a type environment with a new variable name and typescheme
    extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
    extend (TypeEnv env) (x,s) = TypeEnv $ Map.insert x s env


    class Substitutable a where
        apply :: Subst -> a -> a
        ftv :: a -> Set.Set TVar

    instance Substitutable Type where
        apply _ IntT = IntT
        apply _ BoolT = BoolT
        apply s t@(VarT a) = Map.findWithDefault t a s
        apply s (FnT t1 t2) = FnT (apply s t1) (apply s t2)

        ftv (VarT a) = Set.singleton a
        ftv (IntT) = Set.empty
        ftv (BoolT) = Set.empty
        ftv (FnT a b) = ftv a <> ftv b

    instance Substitutable Scheme where
        -- s::Subst, as::[TVar]
        apply s (Forall as t)    = Forall as (apply s' t)
                                    where s' = foldr Map.delete s as 

        ftv (Forall as ty) = ftv ty `Set.difference` Set.fromList as

    instance Substitutable a => Substitutable [a] where
        apply = fmap . apply -- Map the substitution to all in list
        ftv = foldr (Set.union . ftv) Set.empty -- Union of all free variables in list

    instance Substitutable TypeEnv where
        apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
        ftv (TypeEnv env) = ftv $ Map.elems env

    -- Merge 2 substitutions together
    compose:: Subst -> Subst -> Subst
    s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

    -- return the substitution that unifies 2 types, 
    unify :: Type -> Type -> Infer Subst
    unify  (FnT a b) (FnT a' b') = do
        s1 <- unify a a'
        s2 <- unify (apply s1 b) (apply s1 b')
        return (s2 `compose` s1)
    unify (VarT a) t = bind a t
    unify t (VarT a) = bind a t
    unify t1 t2 | t1 == t2 = return Map.empty
                | otherwise = throwError $ UnificationFail t1 t2

    -- 
    -- reminder (from syntax.hs):   newtype TVar = TV String deriving (Show, Eq)
    --                              VarT TVar
    bind :: TVar -> Type -> Infer Subst
    bind a t    | t == VarT a       = return Map.empty -- t and a represent the same type variable
                | occursCheck a t   = throwError $ InfiniteType a t -- a is a free variable of t, which is not allowed
                | otherwise         = return $ Map.singleton a t -- 

    -- Check if the type variable 'a' is a free variable of t
    occursCheck :: Substitutable a => TVar -> a -> Bool
    occursCheck a t = a `Set.member` ftv t

    -- A lazily evaluated stream of new variable names, a-z aa-az ba-bz aaa-aaz and so on
    letters :: [String]
    letters = [1..] >>= flip replicateM ['a'..'z']

    -- Get a new fresh type variable by incrementing on the Unique (state monad) counter
    -- count :: Unique -> Int
    fresh :: Infer Type
    fresh = do
        s <- get
        put s{count = count s + 1} -- Set the new count to be the old count +1 
        return (VarT (TV (letters !! count s)))

    -- Replaces the quantified variables in type scheme with fresh variables in the output type
    -- i.e. Forall [TV "a", TV "b"] (FnT (VarT (TV "a")) VarT (TV "b"))) becomes FnT (VarT (TV "k")) (Var T (TV "l")) 
    instantiate :: Scheme -> Infer Type
    instantiate (Forall as t) = do
        as' <- mapM (const fresh) as
        let s = Map.fromList $ zip as as'
        return $ apply s t

    generalize :: TypeEnv -> Type -> Scheme
    generalize env t = Forall as t
        where as = Set.toList $ ftv t `Set.difference` ftv env
 
    -- Look up a variable in a type environment
    lookupEnv ::  TypeEnv -> Name -> Infer (Subst, Type)
    lookupEnv (TypeEnv env) x = case Map.lookup x env of
        Nothing -> throwError $  UnboundVariable (show x)
        Just s ->   do  t <- instantiate s
                        return (Map.empty, t)

    infer :: TypeEnv -> Expr -> Infer (Subst, Type)
    infer env (Var x) =         lookupEnv env x
    infer env (Int _) =         return (nullSubst, IntT)
    infer env (Bool _) =        return (nullSubst, BoolT)
    infer env (Let x e1 e2) =   do
                                    (s1, t1) <- infer env e1
                                    let env2 = apply s1 env
                                    let t' = generalize env2 t1
                                    (s2, t2) <- infer (env2 `extend` (x, t')) e2
                                    return (s2 `compose` s1, t2)
    infer env (Fn x e) =        do
                                    tv <- fresh
                                    let env' = env `extend` (x, Forall [] tv)
                                    (s1, t1) <- infer env' e
                                    return (s1, (FnT (apply s1 tv) t1))
    infer env (App e1 e2) =     do
                                    (s1, t1) <- infer env e1
                                    (s2, t2) <- infer (apply s1 env) e2
                                    tv <- fresh
                                    s3 <- unify (apply s2 t1) (FnT t2 tv)
                                    return (s3 `compose` s2 `compose` s1, apply s3 tv)                             
    infer env (If e1 e2 e3) =   do
                                    (s1, t1) <- infer env e1
                                    (s2, t2) <- infer env e2
                                    (s3, t3) <- infer env e3
                                    s4 <- unify t1 BoolT
                                    s5 <- unify t2 t3
                                    return (foldl compose s1 [s5, s4, s3, s2], apply s5 t2)                       
    infer env (Op op e1 e2) =   do
                                    (s1, t1) <- infer env e1
                                    (s2, t2) <- infer env e2
                                    tv <- fresh
                                    s3 <- unify (FnT t1 (FnT t2 tv)) (getOpT op)
                                    return (s1 `compose` s2 `compose` s3, apply s3 tv)

    initUnique :: Unique
    initUnique = Unique {count = 0}

    normalize :: Scheme -> Scheme
    normalize (Forall ts body) = Forall (fmap snd ord) (normalizeType body)
        where
            ord = zip (nub $ fv body) (fmap TV letters)

            fv (VarT a) = [a]
            fv (FnT a b) = fv a ++ fv b
            fv (BoolT) = []
            fv (IntT) = []

            normalizeType (FnT a b) = FnT (normalizeType a) (normalizeType b)
            normalizeType (VarT a) = case lookup a ord of
                                        Just x -> VarT x
                                        Nothing -> error "type variable not in signature"
            normalizeType (BoolT) = BoolT
            normalizeType (IntT) = IntT


    runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
    runInfer m = case evalState (runExceptT m) initUnique of 
        Left err -> Left err
        Right (sub, ty) -> Right $ (normalize $ generalize (TypeEnv Map.empty) (apply sub ty))
