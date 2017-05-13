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

type IsVar a = (Eq a, Hashable a)

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
  | forall to. Lam (f a) (Pattern f a to) (f to)
  | Record (LookupList Label (f a))
  | Variant (Variant f a)
  | Prim Prim

data PatRecord f from to where
  PatRecordNil :: PatRecord f a a
  PatRecordCons :: Label -> Pattern f a b -> PatRecord f b c -> PatRecord f a c

data PatVariant f from to = MkPatVariant
  { patVarLabel :: Label
  , patVarPattern :: Pattern f from to
  , patVarImplicit :: Bool
  }

data Pattern f from to where
  PatDefault :: Binder -> Pattern f from (Var from)
  PatTyped :: Pattern f from to -> f from -> Pattern f from to
  PatPrim :: Prim -> Pattern f from from
  PatVariant :: PatVariant f from to -> Pattern f from to
  PatRecord :: PatRecord f from to -> Pattern f from to

data CaseAlt f from = forall to. CaseAlt
  { caseAltPat :: Pattern f from to
  , caseAltBody :: f to
  }

data LetArgs f from to where
  LetArgsNil :: LetArgs f a a
  LetArgsCons :: Type a -> Pattern f a b -> LetArgs f b c -> LetArgs f a c

data Let f from = forall to. MkLet
  { letName :: Binder
  , letArgs :: LetArgs f from to
  , letRetType :: f to
  , letRetTypeHidden :: Bool
  , letBody :: f to
  , letRest :: f (Var to)
  }

data Case f from to where
  CaseNil :: Fwd (CaseAlt f to) -> Case f from to
  CaseCons :: f from -> Case f from (Var to) -> Case f from to

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
  EnvWeaken :: Env a b -> Weaken b (Var c) -> Env a (Var c)
  -- ^
  -- Weakens the expression after applying the given environment.
  --
  -- We want at least one weakening here, otherwise this should
  -- disappear.

-- Eval
-- --------------------------------------------------------------------

data Elim a
  = ElimApp (Exp a)
  | ElimPrimOp
      PrimOp
      (Bwd Prim) -- The evaluated arguments
      (Fwd (Exp a)) -- The yet-to-be-evaluated arguments
  | ElimProj Label
  | ElimCase
      (Env TopLevel a)
      (Fwd (CaseAlt Exp a))

data Eval a
  = EvalCanonical (Canonical Exp a)
  | EvalNeutral a (Bwd (Elim a))

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
envAbs v env = envCons v (var (B (binderVar v))) (EnvWeaken env (WeakenSucc WeakenZero))

envComp :: Env a b -> Env b c -> Env a c
envComp = EnvComp

evalEnv :: Env from to -> NormalEnv from to
evalEnv = \case
  EnvNormal env -> env
  EnvComp env1 env2 -> goComp (evalEnv env1) env2
  EnvWeaken env wk -> goWeaken env wk
  where
    goComp :: NormalEnv a b -> Env b c -> NormalEnv a c
    goComp (EnvNil wk) env2 = goCompWeaken wk env2
    goComp (EnvCons v1 e1 env1) env2 = EnvCons v1 (susp env2 e1) (EnvComp env1 env2)

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
      EnvCons v e env -> EnvCons v (susp (EnvNormal (EnvNil wk)) e) $ case wk of
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
