module Swine.Exp where

import           Swine.Prelude
import qualified Swine.LookupList as LL

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
  PatIgnore (Binder s) -> Binder ("v_" <> s)

data Var a
  = B Binder
  | F a

type Label = Text

type IsVar a = (Eq a, Hashable a)

-- Expressions
-----------------------------------------------------------------------

var :: a -> Exp a
var v = Syntax (Var v)

lam :: Pattern -> Exp (Var a) -> Exp a
lam v body = Syntax (Canonical (Lam v body))

prim :: Prim -> Exp a
prim p = Syntax (Canonical (Prim p))

data Prim
  = PrimInt64 Int64
  deriving (Eq, Ord, Show, Read)

data PrimOp
  = PrimOpPlusInt64
  deriving (Eq, Ord, Show, Read)

type Record a = LookupList Label (Exp a)

data Canonical a
  = Prim Prim
  | Lam Pattern (Exp (Var a))
  | Record (Record a)
  | Variant Label (Exp a)

data CaseAlt a
  = CaseAltVariant Label Pattern (Exp (Var a))
  | CaseAltPrim Prim (Exp a)
  | CaseAltDefault Pattern (Exp (Var a))

data Elim a
  = ElimApp (Exp a)
  | ElimPrimOp
      PrimOp
      (Bwd Prim) -- The evaluated arguments
      (Fwd (Exp a)) -- The yet-to-be-evaluated arguments
  | ElimProj
      Label
  | ElimCase
      (Fwd (CaseAlt a))

-- We call this "syntax" because the constructor roughly reflect
-- what expressions the user can form.
data Syntax a where
  Var :: a -> Syntax a
  Canonical :: Canonical a -> Syntax a
  App :: Exp a -> Exp a -> Syntax a
  Let :: Pattern -> Exp a -> Exp (Var a) -> Syntax a
  -- ^ A set of mutually recursive bindings
  PrimOp :: PrimOp -> Fwd (Exp a) -> Syntax a
  -- ^ Primitive operations are always fully saturated.
  -- INVARIANT: If we have @PrimOp pop args@, then
  -- @length args == primOpArity pop@.
  Proj :: Exp a -> Label -> Syntax a
  Case :: Exp a -> Fwd (CaseAlt a) -> Syntax a

data Exp a where
  Syntax :: Syntax a -> Exp a
  Susp :: Env b a -> Syntax b -> Exp a
  -- Note that we can trivially always have syntax here since we have
  -- EnvComp
  -- TODO it would be good to constraint this to a non-nil susp

susp :: Env b a -> Exp b -> Exp a
susp env = \case
  Syntax e -> Susp env e
  Susp env' e -> Susp (EnvComp env' env) e

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

-- Transformation of expressions
-----------------------------------------------------------------------

expMap_ :: (ExpMap f) => (forall a. Exp a -> Exp a) -> f b -> f b
expMap_ f x = runIdentity (expMap (Identity . f) x)

class ExpMap f where
  expMap ::
       (Applicative m)
    => (forall a. Exp a -> m (Exp a))
    -> f b
    -> m (f b)

instance ExpMap Exp where
  expMap f = f

instance ExpMap Elim where
  expMap f = \case
    ElimApp e -> ElimApp <$> f e
    ElimPrimOp pop evald unevald ->
      ElimPrimOp pop evald <$> for unevald (expMap f)
    ElimProj label -> pure (ElimProj label)
    ElimCase alts -> ElimCase <$> for alts (expMap f)

instance ExpMap Canonical where
  expMap f = \case
    Lam pat body -> Lam pat <$> f body
    Prim p -> pure (Prim p)
    Record rec -> Record <$> for rec f
    Variant lbl e -> Variant lbl <$> f e

instance ExpMap CaseAlt where
  expMap f = \case
    CaseAltVariant lbl pat body -> CaseAltVariant lbl pat <$> f body
    CaseAltDefault pat body -> CaseAltDefault pat <$> f body
    CaseAltPrim p body -> CaseAltPrim p <$> f body

-- Evaluation
-----------------------------------------------------------------------

data EvalError
  = forall a. EEBadCanonicalForApp (Canonical a) (Exp a)
  | forall a. EEBadCanonicalForPrimOp PrimOp (Canonical a)
  | EEBadArgsForPrimOp PrimOp (Fwd Prim)
  | forall a. EERecordLabelNotFound (Record a) Label
  | forall a. EEBadCanonicalForProj (Canonical a) Label
  | forall a. EENoMatchingAlternative (Canonical a) (Fwd (CaseAlt a))
  | forall a. EEBadCanonicalForCase (Canonical a) (Fwd (CaseAlt a))

data Eval a
  = EvalCanonical (Canonical a)
  | EvalNeutral a (Bwd (Elim a))

evalToSyntax :: Eval a -> Syntax a
evalToSyntax = \case
  EvalCanonical c -> Canonical c
  EvalNeutral h els -> goNeutral h els
  where
    goNeutral :: a -> Bwd (Elim a) -> Syntax a
    goNeutral v = \case
      BwdNil -> Var v
      els :> el -> case el of
        ElimApp arg -> App (Syntax (goNeutral v els)) arg
        ElimPrimOp pop evald unevald ->
          PrimOp pop
            (bwdReverse (map (Syntax . Canonical . Prim) evald) <> (Syntax (goNeutral v els) :< unevald))
        ElimProj label ->
          Proj (Syntax (goNeutral v els)) label
        ElimCase alts ->
          Case (Syntax (goNeutral v els)) alts

-- Pushes the Susp down into the expression.
removeSusp :: Exp a -> Syntax a
removeSusp = \case
  Syntax e -> e
  Susp env e0 -> case e0 of
    Var v -> removeSusp (envLookup env v)
    Canonical (Lam v body) -> Canonical (Lam v (susp (envLam v env) body))
    Canonical (Prim p) -> Canonical (Prim p)
    Canonical (Record rec) -> Canonical (Record (map (susp env) rec))
    Canonical (Variant lbl e) -> Canonical (Variant lbl (susp env e))
    App fun arg -> App (susp env fun) (susp env arg)
    PrimOp pop args -> PrimOp pop (map (susp env) args)
    Let pat e1 e2 -> Let pat (susp env e1) (susp (envLam pat env) e2)
    Proj e lbl -> Proj (susp env e) lbl
    Case e alts -> Case (susp env e) (map (suspAlt env) alts)
  where
    suspAlt env = \case
      CaseAltVariant lbl pat body -> CaseAltVariant lbl pat (susp (envLam pat env) body)
      CaseAltDefault pat body -> CaseAltDefault pat (susp (envLam pat env) body)
      CaseAltPrim p body -> CaseAltPrim p (susp env body)

eval :: forall a. Syntax a -> Either EvalError (Eval a)
eval = \case
  Var v -> return (EvalNeutral v mempty)
  Canonical c -> return (EvalCanonical c)
  App fun0 arg -> do
    fun <- eval (removeSusp fun0)
    case fun of
      EvalNeutral h args -> return (EvalNeutral h (args :> ElimApp arg))
      EvalCanonical c -> case c of
        Lam v body -> eval (removeSusp (susp (envCons v arg envNil) body))
        p@Prim{} -> Left (EEBadCanonicalForApp p arg)
        r@Record{} -> Left (EEBadCanonicalForApp r arg)
        v@Variant{} -> Left (EEBadCanonicalForApp v arg)
  PrimOp pop args00 -> do
    -- TODO we chose to be lazy here and evaluate one-by-one. This is
    -- to be consistent with eventual short-circuiting, but pretty
    -- arbitrary. Decide in a more principled way
    let go :: Bwd Prim -> Fwd (Exp a) -> Either EvalError (Eval a)
        go prevArgs = \case
          FwdNil -> EvalCanonical . Prim <$> evalPrimOp pop prevArgs
          arg0 :< args0 -> do
            arg <- eval (removeSusp arg0)
            case arg of
              EvalNeutral h args -> do
                let el = ElimPrimOp pop prevArgs args0
                return (EvalNeutral h (args :> el))
              EvalCanonical canon -> case canon of
                Prim p -> go (prevArgs :> p) args0
                e@Lam{} -> Left (EEBadCanonicalForPrimOp pop e)
                r@Record{} -> Left (EEBadCanonicalForPrimOp pop r)
                v@Variant{} -> Left (EEBadCanonicalForPrimOp pop v)
    go BwdNil args00
  Let v e1 e2 -> eval (removeSusp (susp (envCons v e1 envNil) e2))
  Proj e0 lbl -> do
    e <- eval (removeSusp e0)
    case e of
      EvalNeutral h args -> return (EvalNeutral h (args :> ElimProj lbl))
      EvalCanonical c -> case c of
        Record rec -> case LL.lookup rec lbl of
          Just e' -> eval (removeSusp e')
          Nothing -> Left (EERecordLabelNotFound rec lbl)
        l@Lam{} -> Left (EEBadCanonicalForProj l lbl)
        p@Prim{} -> Left (EEBadCanonicalForProj p lbl)
        v@Variant{} -> Left (EEBadCanonicalForProj v lbl)
  Case e0 alts0 -> do
    e <- eval (removeSusp e0)
    case e of
      EvalNeutral h args -> return (EvalNeutral h (args :> ElimCase alts0))
      EvalCanonical c -> case c of
        Variant lbl arg -> do
          let go :: Fwd (CaseAlt a) -> Either EvalError (Eval a)
              go = \case
                FwdNil -> Left (EENoMatchingAlternative (Variant lbl arg) alts0)
                alt :< alts -> case alt of
                  CaseAltDefault pat body ->
                    -- We put e0 on purpose to minimize evaluations shown to the
                    -- user
                    eval (removeSusp (susp (envCons pat e0 envNil) body))
                  CaseAltVariant lbl' pat body ->
                    if lbl == lbl'
                      then eval (removeSusp (susp (envCons pat arg envNil) body))
                      else go alts
                  CaseAltPrim{} -> Left (EEBadCanonicalForCase (Variant lbl arg) alts0)
          go alts0
        Prim p -> do
          let go :: Fwd (CaseAlt a) -> Either EvalError (Eval a)
              go = \case
                FwdNil -> Left (EENoMatchingAlternative (Prim p) alts0)
                alt :< alts -> case alt of
                  CaseAltDefault pat body ->
                    -- We put e0 on purpose to minimize evaluations shown to the
                    -- user
                    eval (removeSusp (susp (envCons pat e0 envNil) body))
                  CaseAltVariant{} ->
                    Left (EEBadCanonicalForCase (Prim p) alts0)
                  CaseAltPrim p' body -> if p == p'
                    then eval (removeSusp body)
                    else go alts
          go alts0
        l@Lam{} -> Left (EEBadCanonicalForCase l alts0)
        r@Record{} -> Left (EEBadCanonicalForCase r alts0)

-- Gets rid of all substitutions, without performing any evaluation.
-- Useful to print out parseable expressions.
removeAllSusps :: Exp a -> Syntax a
removeAllSusps e0 = case removeSusp e0 of
  Var v -> Var v
  Canonical c -> case c of
    Prim p -> Canonical (Prim p)
    Lam pat body -> Canonical (Lam pat (Syntax (removeAllSusps body)))
    Record rec -> Canonical (Record (map (Syntax . removeAllSusps) rec))
    Variant lbl e -> Canonical (Variant lbl (Syntax (removeAllSusps e)))
  App fun arg -> App (Syntax (removeAllSusps fun)) (Syntax (removeAllSusps arg))
  Let pat e1 e2 -> Let pat (Syntax (removeAllSusps e1)) (Syntax (removeAllSusps e2))
  PrimOp pop args -> PrimOp pop (map (Syntax . removeAllSusps) args)
  Proj e lbl -> Proj (Syntax (removeAllSusps e)) lbl
  Case e alts -> Case
    (Syntax (removeAllSusps e))
    (map
      (\case
        CaseAltVariant lbl pat body -> CaseAltVariant lbl pat (Syntax (removeAllSusps body))
        CaseAltDefault pat body -> CaseAltDefault pat (Syntax (removeAllSusps body))
        CaseAltPrim p body -> CaseAltPrim p (Syntax (removeAllSusps body)))
      alts)

-- Primitive operations
-----------------------------------------------------------------------

primOpArity :: PrimOp -> Int
primOpArity = \case
  PrimOpPlusInt64 -> 2

evalPrimOp :: PrimOp -> Bwd Prim -> Either EvalError Prim
evalPrimOp pop args = case (pop, toList args) of
  (PrimOpPlusInt64, [PrimInt64 x, PrimInt64 y]) -> return (PrimInt64 (x+y))
  _ -> Left (EEBadArgsForPrimOp pop (bwdReverse args))

-- Instances
-----------------------------------------------------------------------

deriving instance (Eq a) => Eq (Var a)
deriving instance Generic (Var a)
instance (Hashable a) => Hashable (Var a)

instance Eq Binder where
  _ == _ = True
instance Hashable Binder where
  hashWithSalt s _ = hashWithSalt s ()
