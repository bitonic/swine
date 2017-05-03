module Swine.Exp where

import           Swine.Prelude

-- Variables
-----------------------------------------------------------------------

newtype Binder = Binder Text

data Pattern
  = PatBind Binder
  | PatIgnore Binder -- Without the _

-- Useful when we need generated names
patBinder :: Pattern -> Binder
patBinder = \case
  PatBind v -> v
  PatIgnore (Binder s) -> Binder ("ign_" <> s)

data Var a
  = B Binder
  | F a

type Label = Text

type IsVar a = (Eq a, Hashable a)

-- Expressions
-----------------------------------------------------------------------

-- TODO do the primops as an eliminator storing the already evaluated
-- canonicals and the to-evaluate expressions in the eliminator.

var :: a -> Exp a
var h = Evaluated (Neutral h mempty)

lam :: Pattern -> Exp (Var a) -> Exp a
lam v body = Evaluated (Canonical (Lam v body))

prim :: Prim -> Exp a
prim p = Evaluated (Canonical (Prim p))

data Prim
  = PrimInt64 Int64
  deriving (Eq, Ord, Show, Read)

data Canonical a
  = Prim Prim
  | Lam Pattern (Exp (Var a))

data Evaluated a = Canonical (Canonical a) | Neutral a (Bwd (Exp a))

data Exp a where
  Evaluated :: Evaluated a -> Exp a
  -- Yet to be evaluated
  App :: Exp a -> Exp a -> Exp a
  Let :: Pattern -> Exp a -> Exp (Var a) -> Exp a
  Susp :: Env b a -> Exp b -> Exp a
  -- TODO it would be good to constraint this to a non-nil susp

-- Environment
-----------------------------------------------------------------------

data Weaken from to where
  WeakenZero :: Weaken a a
  WeakenSucc :: Weaken from to -> Weaken from (Var to)

data NormalEnv from to where
  EnvNil :: Weaken from to -> NormalEnv from to
  -- ^ Possibly weakens an expression
  EnvCons :: Pattern -> Exp to -> Env from to -> NormalEnv (Var from) to
  -- ^ Adds a variable to an environment

data Env from to where
  EnvNormal :: NormalEnv from to -> Env from to
  EnvComp :: Env a b -> Env b c -> Env a c
  -- ^ Composes two environments together
  EnvWeaken :: Env a b -> Weaken b (Var c) -> Env a (Var c)
  -- ^
  -- Weakens the expression after applying the given environment.
  --
  -- We want at least one weakening here, otherwise this should
  -- disappear.

-- API

envNil :: Env a a
envNil = EnvNormal (EnvNil WeakenZero)

envCons :: Pattern -> Exp to -> Env from to -> Env (Var from) to
envCons v e env = EnvNormal (EnvCons v e env)

envLookup :: Env from to -> from -> Exp to
envLookup env0 v = case evalEnv env0 of
  EnvNil wk -> var (goWeaken wk v) -- TODO I don't know if this makes sense, test for this specifically
  EnvCons _b e env -> case v of
    B _b -> e
    F v' -> envLookup env v'
  where
    goWeaken :: Weaken from to -> from -> to
    goWeaken wk0 v' = case wk0 of
      WeakenZero -> v'
      WeakenSucc wk -> F (goWeaken wk v')

envLam :: Pattern -> Env from to -> Env (Var from) (Var to)
envLam v env = envCons v (var (B (patBinder v))) (EnvWeaken env (WeakenSucc WeakenZero))

envComp :: Env a b -> Env b c -> Env a c
envComp = EnvComp

-- env "normalization"

evalEnv :: Env from to -> NormalEnv from to
evalEnv = \case
  EnvNormal env -> env
  EnvComp env1 env2 -> goComp (evalEnv env1) env2
  EnvWeaken env wk -> goWeaken env wk
  where
    goComp :: NormalEnv a b -> Env b c -> NormalEnv a c
    goComp (EnvNil wk) env2 = goCompWeaken wk env2
    goComp (EnvCons v1 e1 env1) env2 = EnvCons v1 (Susp env2 e1) (EnvComp env1 env2)

    goWeaken :: Env a b -> Weaken b c -> NormalEnv a c
    goWeaken env0 = \case
      WeakenZero -> evalEnv env0
      wk@(WeakenSucc _) -> case env0 of
        EnvNormal env -> goWeakenNormal env wk
        EnvComp env1 env2 -> goComp (evalEnv env1) (EnvWeaken env2 wk)
        EnvWeaken env wk' -> goWeaken env (compWeaken wk' wk)

    goWeakenNormal :: NormalEnv a b -> Weaken b c -> NormalEnv a c
    goWeakenNormal env0 wk = case env0 of
      EnvNil wk' -> EnvNil (compWeaken wk' wk)
      EnvCons v e env -> EnvCons v (Susp (EnvNormal (EnvNil wk)) e) $ case wk of
        WeakenZero -> env
        WeakenSucc _ -> EnvWeaken env wk

    goCompWeaken :: Weaken a b -> Env b c -> NormalEnv a c
    goCompWeaken wk = \case
      EnvNormal (EnvNil wk') -> EnvNil (compWeaken wk wk')
      EnvNormal (EnvCons v e env) -> case wk of
        WeakenZero -> EnvCons v e env
        WeakenSucc wk' -> goCompWeaken wk' env
      EnvComp env1 env2 -> goComp (goCompWeaken wk env1) env2
      EnvWeaken env wk' -> goWeakenNormal (goCompWeaken wk env) wk' -- TODO can we do this more lazily?

    compWeaken :: Weaken a b -> Weaken b c -> Weaken a c
    compWeaken wk1 WeakenZero = wk1
    compWeaken wk1 (WeakenSucc wk2) = WeakenSucc (compWeaken wk1 wk2)

-- Evaluation
-----------------------------------------------------------------------

data EvalError
  = forall a. EEBadApp (Canonical a) (Exp a)

eval :: Exp a -> Either EvalError (Evaluated a)
eval = \case
  Evaluated n -> return n
  App fun0 arg -> do
    fun <- eval fun0
    case fun of
      Neutral h args -> return (Neutral h (args :> arg))
      Canonical (Lam v body) -> eval (removeSusp (envCons v arg envNil) body)
      Canonical p@(Prim _) -> Left (EEBadApp p arg)
  Let v e1 e2 -> eval (removeSusp (envCons v e1 envNil) e2)
  Susp env e -> eval (removeSusp env e)

-- Removes all suspensions
removeAllSusps :: Exp a -> Either EvalError (Exp a)
removeAllSusps = \case
  Evaluated (Neutral h args) -> Evaluated . Neutral h <$> mapM removeAllSusps args
  Evaluated (Canonical (Lam pat body)) ->
    Evaluated . Canonical . Lam pat <$> removeAllSusps body
  Evaluated (Canonical (Prim p)) -> return (Evaluated (Canonical (Prim p)))
  App fun arg -> App <$> removeAllSusps fun <*> removeAllSusps arg
  Let pat e1 e2 -> Let pat <$> removeAllSusps e1 <*> removeAllSusps e2
  Susp env e -> removeAllSusps (removeSusp env e)

-- Pushes the Susp down into the expression, then does something
-- to the leaves
removeSusp :: Env from to -> Exp from -> Exp to
removeSusp env = \case
  Evaluated e0 -> case e0 of
    Neutral h args -> foldl' App (envLookup env h) (map (Susp env) args)
    Canonical (Lam v body) -> Evaluated (Canonical (Lam v (Susp (envLam v env) body)))
    Canonical (Prim p) -> Evaluated (Canonical (Prim p))
  App fun arg -> App (Susp env fun) (Susp env arg)
  Let v e1 e2 -> Let v (Susp env e1) (Susp (envLam v env) e2)
  Susp env' e -> removeSusp (envComp env' env) e

-- Instances
-----------------------------------------------------------------------

deriving instance (Eq a) => Eq (Var a)
deriving instance Generic (Var a)
instance (Hashable a) => Hashable (Var a)

instance Eq Binder where
  _ == _ = True
instance Hashable Binder where
  hashWithSalt s _ = hashWithSalt s ()

