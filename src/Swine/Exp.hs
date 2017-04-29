{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Swine.Exp where

import           Swine.Prelude

-- Variables
-----------------------------------------------------------------------

newtype Binder = Binder Text

instance Eq Binder where
  _ == _ = True

data Var a
  = B Binder
  | F a

type Label = Text

-- Expressions
-----------------------------------------------------------------------

var :: a -> Exp a
var h = HNF (Neutral h [])

var_ :: a -> HNF a
var_ h = Neutral h []

data HNF a
  = Neutral a [Exp a]
  | Lam Binder (Exp (Var a))

data Exp a where
  HNF :: HNF a -> Exp a
  App :: Exp a -> Exp a -> Exp a
  Susp :: Env b a -> Exp b -> Exp a

-- Environment
-----------------------------------------------------------------------

data Weaken from to where
  WNil :: Weaken a a
  WWeaken :: Weaken from to -> Weaken from (Var to)

data NormalEnv from to where
  ENil :: Weaken from to -> NormalEnv from to
  ECons :: Binder -> Exp to -> Env from to -> NormalEnv (Var from) to

data Env from to where
  ENormal :: NormalEnv from to -> Env from to
  EComp :: Env a b -> Env b c -> Env a c
  EWeaken :: Env a b -> Weaken b c -> Env a c

-- API

envNil :: Env a a
envNil = ENormal (ENil WNil)

envCons :: Binder -> Exp to -> Env from to -> Env (Var from) to
envCons v e env = ENormal (ECons v e env)

envLookup :: Env from to -> from -> Exp to
envLookup env0 v = case evalEnv env0 of
  ENil wk -> var (goWeaken wk v) -- TODO I don't know if this makes sense, test for this specifically
  ECons _b e env -> case v of
    B _b -> e
    F v' -> envLookup env v'
  where
    goWeaken :: Weaken from to -> from -> to
    goWeaken wk0 v' = case wk0 of
      WNil -> v'
      WWeaken wk -> F (goWeaken wk v')

envLam :: Binder -> Env from to -> Env (Var from) (Var to)
envLam v env = envCons v (var (B v)) (EWeaken env (WWeaken WNil))

envComp :: Env a b -> Env b c -> Env a c
envComp = EComp

-- env "normalization"

evalEnv :: Env from to -> NormalEnv from to
evalEnv = \case
  ENormal env -> env
  EComp env1 env2 -> goComp (evalEnv env1) env2
  EWeaken env wk -> goWeaken env wk
  where
    goComp :: NormalEnv a b -> Env b c -> NormalEnv a c
    goComp (ENil wk) env2 = goCompWeaken wk env2
    goComp (ECons v1 e1 env1) env2 = ECons v1 (Susp env2 e1) (EComp env1 env2)

    goWeaken :: Env a b -> Weaken b c -> NormalEnv a c
    goWeaken env0 wk = case env0 of
      ENormal (ENil wk') -> ENil (compWeaken wk' wk)
      ENormal (ECons v e env) -> ECons v (Susp (ENormal (ENil wk)) e) (EWeaken env wk)
      EComp env1 env2 -> goComp (evalEnv env1) (EWeaken env2 wk)
      EWeaken env wk' -> goWeaken env (compWeaken wk' wk)

    goCompWeaken :: Weaken a b -> Env b c -> NormalEnv a c
    goCompWeaken wk = \case
      ENormal (ENil wk') -> ENil (compWeaken wk wk')
      ENormal (ECons v e env) -> case wk of
        WNil -> ECons v e env
        WWeaken wk' -> goCompWeaken wk' env
      EComp env1 env2 -> goComp (goCompWeaken wk env1) env2
      EWeaken env wk' -> goWeaken (ENormal (goCompWeaken wk env)) wk' -- TODO can we do this more lazily?

    compWeaken :: Weaken a b -> Weaken b c -> Weaken a c
    compWeaken wk1 WNil = wk1
    compWeaken wk1 (WWeaken wk2) = WWeaken (compWeaken wk1 wk2)

-- Evaluation
-----------------------------------------------------------------------

eval :: Exp a -> Either Text (HNF a)
eval = \case
  HNF n -> return n
  App fun0 arg -> do
    fun <- eval fun0
    case fun of
      Neutral h args -> return (Neutral h (args <> [arg]))
      Lam v body -> evalSusp (envCons v arg envNil) body
  Susp env e -> evalSusp env e

evalSusp :: Env from to -> Exp from -> Either Text (HNF to)
evalSusp env = \case
  HNF e0 -> case e0 of
    Neutral h args -> eval (foldl' App (envLookup env h) (map (Susp env) args))
    Lam v body -> return (Lam v (Susp (envLam v env) body))
  App fun arg -> eval (App (Susp env fun) (Susp env arg))
  Susp env' e -> evalSusp (envComp env' env) e
