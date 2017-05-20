module Swine.Lang.Exp where

import           Swine.Prelude
import           Swine.Prim
import           Swine.Binder

-- Variables
-----------------------------------------------------------------------

data Var a
  = B Text
  | F a
  deriving (Functor, Foldable, Traversable, Eq, Show, Generic)
instance (Hashable a) => Hashable (Var a)

class (Eq a, Hashable a) => IsVar a where
  topLevelWeaken :: Weaken TopLevel a

-- Expressions
-- --------------------------------------------------------------------

data Telescope f a
  = TelescopeNil
  | TelescopeCons Label (f a) (Telescope f (Var a))

data Variant f a = MkVariant
  { variantLabel :: Label
  , variantBody :: f a
  , variantBodyImplicit :: Bool
  }

data Canonical f a
  = Type
  | LamType Binder (f a) (f (Var a))
  | RecordType (Telescope f a)
  | VariantType (LookupList Label (f a))
  | PrimType PrimType
  | forall to. Lam (f a) (Pattern 'True f a to) (f to)
  | Record (LookupList Label (f a))
  | Variant (Variant f a)
  | Prim Prim

data PatRecord irr f from to where
  PatRecordNil :: PatRecord irr f a a
  PatRecordCons :: Label -> Pattern irr f a b -> PatRecord irr f b c -> PatRecord irr f a c

data PatVariant irr f from to = MkPatVariant
  { patVarLabel :: Label
  , patVarPattern :: Pattern irr f from to
  , patVarImplicit :: Bool
  }

data Pattern (irr :: Bool) f from to where
  PatDefault :: Binder -> Pattern irr f from (Var from)
  PatTyped :: Pattern irr f from to -> f from -> Pattern irr f from to
  PatPrim :: Prim -> Pattern 'False f from from
  PatVariant :: PatVariant irr f from to -> Pattern 'False f from to
  PatRecord :: PatRecord irr f from to -> Pattern irr f from to

data CaseAlt f from = forall to. CaseAlt
  { caseAltPat :: Pattern 'False f from to
  , caseAltBody :: f to
  }

data LetArgs f from to where
  LetArgsNil :: LetArgs f a a
  LetArgsCons :: f a -> Pattern 'True f a b -> LetArgs f b c -> LetArgs f a c

data Let f from = forall to. MkLet
  { letName :: Binder
  , letArgs :: LetArgs f from to
  , letRetType :: f to
  , letRetTypeHidden :: Bool
  , letBody :: f to
  , letRest :: f (Var from)
  }

data Case f from to where
  CaseNil :: Fwd (CaseAlt f to) -> Case f from to
  CaseCons :: Binder -> f from -> Case f from (Var to) -> Case f from to

data Syntax f a
  = Canonical (Canonical f a)
  | Var a
  | App (f a) (f a)
  | Proj (f a) Label
  | Case
      (f a) -- The scrutinized
      (Case f a TopLevel) -- The sealed case
  | Let (Let f a)
  | PrimOp PrimOp (Fwd (f a))

class HasSyntax f where
  injSyntax :: Syntax f a -> f a

data NoSusps a = NoSusps (Syntax NoSusps a)

instance HasSyntax NoSusps where
  injSyntax = NoSusps

canonical :: (HasSyntax f) => Canonical f a -> f a
canonical c = injSyntax (Canonical c)

var :: (HasSyntax f) => a -> f a
var  = injSyntax . Var

app :: (HasSyntax f) => f a -> f a -> f a
app l r = injSyntax (App l r)

proj :: (HasSyntax f) => f a -> Label -> f a
proj e lbl = injSyntax (Proj e lbl)

primOp :: (HasSyntax f) => PrimOp -> Fwd (f a) -> f a
primOp pop args = injSyntax (PrimOp pop args)

case_ :: (HasSyntax f) => f a -> Case f a TopLevel -> f a
case_ a b = injSyntax (Case a b)

let_ :: (HasSyntax f) => Let f a -> f a
let_ l = injSyntax (Let l)

type Type = Exp

data Exp a
  = Syntax (Syntax Exp a)
  | forall b. Susp (Env b a) (Syntax Exp b)

instance HasSyntax Exp where
  injSyntax = Syntax

susp :: Env b a -> Exp b -> Exp a
susp env = \case
  Syntax e -> Susp env e
  Susp env' e -> Susp (envComp env' env) e

data Weaken from to where
  WeakenZero :: Weaken a a
  WeakenSucc :: Weaken from to -> Weaken from (Var to)

data NormalEnv from to where
  EnvNil :: Weaken from to -> NormalEnv from to
  -- ^ Possibly weakens an expression
  EnvCons :: Binder -> Exp to -> Env from to -> NormalEnv (Var from) to
  -- ^ Adds a variable to an environment

data Env from to where
  EnvNormal :: NormalEnv from to -> Env from to
  EnvComp :: Env a b -> Env b c -> Env a c
  -- ^ Composes two environments together

-- Eval
-- --------------------------------------------------------------------

data Elim a
  = ElimApp (Exp a)
  | ElimPrimOp
      PrimOp
      (Bwd Prim) -- The evaluated arguments
      (Fwd (Exp a)) -- The yet-to-be-evaluated arguments
  | ElimProj Label

data NeutralHead a
  = NHVar a
  | NHCase (Exp a) (Case Exp a TopLevel)
  -- ^ We cannot store the case as an eliminator
  -- because we might be stuck on a case
  -- because of some nested pattern.

data Eval a
  = EvalCanonical (Canonical Exp a)
  | EvalNeutral (NeutralHead a) (Bwd (Elim a))

evalExp :: Eval a -> Exp a
evalExp = \case
  EvalCanonical c -> canonical c
  EvalNeutral nh els -> foldl' appElim (nhExp nh) els
    where
      nhExp = \case
        NHVar v -> var v
        NHCase e cs -> case_ e cs

      appElim e = \case
        ElimApp e' -> app e e'
        ElimPrimOp pop evald unevald ->
          primOp pop (bwdReverse (map (canonical . Prim) evald) <> (e :< unevald))
        ElimProj lbl -> proj e lbl

-- Env toolkit
-- --------------------------------------------------------------------


envNil :: Env a a
envNil = EnvNormal (EnvNil WeakenZero)

envCons :: Binder -> Exp to -> Env from to -> Env (Var from) to
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

envAbs :: Binder -> Env from to -> Env (Var from) (Var to)
envAbs v env = envCons v (var (B (binderVar v))) (envWeaken env)

envWeaken :: Env from to -> Env from (Var to)
envWeaken env = envComp env (EnvNormal (EnvNil (WeakenSucc WeakenZero)))

envComp :: Env a b -> Env b c -> Env a c
envComp = EnvComp

evalEnv :: Env from to -> NormalEnv from to
evalEnv = \case
  EnvNormal env -> env
  EnvComp env1 env2 -> goComp (evalEnv env1) env2
  where
    goComp :: NormalEnv a b -> Env b c -> NormalEnv a c
    goComp (EnvNil wk) env2 = goCompWeaken wk env2
    goComp (EnvCons v1 e1 env1) env2 = EnvCons v1 (susp env2 e1) (EnvComp env1 env2)

    goCompWeaken :: Weaken a b -> Env b c -> NormalEnv a c
    goCompWeaken wk = \case
      EnvNormal (EnvNil wk') -> EnvNil (compWeaken wk wk')
      EnvNormal (EnvCons v e env) -> case wk of
        WeakenZero -> EnvCons v e env
        WeakenSucc wk' -> goCompWeaken wk' env
      EnvComp env1 env2 -> goComp (goCompWeaken wk env1) env2

    compWeaken :: Weaken a b -> Weaken b c -> Weaken a c
    compWeaken wk1 WeakenZero = wk1
    compWeaken wk1 (WeakenSucc wk2) = WeakenSucc (compWeaken wk1 wk2)

{-
-- Instances
-- --------------------------------------------------------------------

deriving instance (Functor f) => Functor (Syntax f)
deriving instance (Functor f) => Functor (Canonical f)
deriving instance (Functor f) => Functor (Telescope f)
deriving instance (Functor f) => Functor (Let f)
instance Functor (LetArgs from) where
  fmap f = \case
    LetArgsNil -> LetArgsNil
-}
