module Swine.Lang.Eval where

import           Swine.Prelude
import           Swine.Binder
import           Swine.Lang.Exp
import           Swine.Lang.RemoveSusp
import qualified Swine.LookupList as LL
import           Swine.Prim

data EvalError
  = forall a. EEBadFunction (Eval a)
  | forall a. EEBadProj (Eval a) Label
  | forall a. EEBadPrimOpArg PrimOp (Canonical Exp a)
  | EEBadPrimOpArgs PrimOp (Fwd Prim)
  | forall a b. EENoMatchingAlternative (Exp a) (Fwd (CaseAlt Exp b))
  | forall a b c irr. EEIllTypedCase (Canonical Exp a) (Pattern irr Exp b c)
  | forall a. EERecordFieldMissing Label (Exp a)

type EvalM = Either EvalError

eval :: forall a. (IsVar a) => Syntax Exp a -> EvalM (Eval a)
eval = \case
  Var v -> return (EvalNeutral (NHVar v) mempty) -- TODO look up def if top level
  Canonical c -> return (EvalCanonical c)
  App fun0 arg -> do
    fun <- eval (removeSusp fun0)
    case fun of
      EvalNeutral h args -> return (EvalNeutral h (args :> ElimApp arg))
      EvalCanonical c0 -> case c0 of
        Lam _ty pat body -> eval (removeSusp (susp (evalIrrPat pat arg) body))
        _ -> Left (EEBadFunction fun)
  Proj e0 lbl -> do
    e <- eval (removeSusp e0)
    case e of
      EvalNeutral h args -> return (EvalNeutral h (args :> ElimProj lbl))
      EvalCanonical c0 -> case c0 of
        Record flds -> case LL.lookup flds lbl of
          Just fldE -> eval (removeSusp fldE)
          Nothing -> Left (EEBadProj e lbl)
        _ -> Left (EEBadProj e lbl)
  Case e0 alts ->
    evalCase e0 (EnvNormal (EnvNil topLevelWeaken)) alts
  Let (MkLet b largs _retTy _hid body rest) ->
    eval (removeSusp (susp (envCons b (letToLam largs body) envNil) rest))
  PrimOp pop args00 -> do
    -- TODO we chose to be lazy here and evaluate one-by-one. This is
    -- to be consistent with eventual short-circuiting, but pretty
    -- arbitrary. Decide in a more principled way
    let go :: Bwd Prim -> Fwd (Exp a) -> EvalM (Eval a)
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
                e -> Left (EEBadPrimOpArg pop e)
    go BwdNil args00

evalIrrPat :: Pattern 'True Exp from to -> Exp from -> Env to from
evalIrrPat pat0 arg = case pat0 of
  PatDefault b -> envCons b arg envNil
  PatTyped pat _ty -> evalIrrPat pat arg
  PatRecord patRec -> evalIrrPatRecord patRec arg

evalIrrPatRecord :: PatRecord 'True Exp from to -> Exp from -> Env to from
evalIrrPatRecord pat0 arg = case pat0 of
  PatRecordNil -> envNil
  PatRecordCons lbl pat patRec ->
    envComp (evalIrrPatRecord patRec (susp (patWeaken pat) arg)) (evalIrrPat pat (proj arg lbl))

patWeaken :: Pattern irr Exp from to -> Env from to
patWeaken = \case
  PatDefault{} -> envWeaken envNil
  PatTyped pat _ty -> patWeaken pat
  PatRecord patRec -> patRecWeaken patRec
  PatVariant (MkPatVariant _lbl pat _impl) -> patWeaken pat
  PatPrim{} -> envNil

patRecWeaken :: PatRecord irr Exp from to -> Env from to
patRecWeaken = \case
  PatRecordNil -> envNil
  PatRecordCons _lbl pat patRec -> envComp (patWeaken pat) (patRecWeaken patRec)

-- Note: this is fine performance wise since the env-building
-- (which we might discard in the end) is constant time for
-- each step.
evalCase ::
     (IsVar a)
  => Exp a -> Env tl a -> Case Exp a tl
  -> EvalM (Eval a)
evalCase e env = \case
  CaseNil alts -> do
    mbRes <- evalCaseAlts e env alts
    case mbRes of
      Nothing -> Left (EENoMatchingAlternative e alts)
      Just res -> return res
  CaseCons b arg cs -> evalCase e (envCons b arg env) cs

evalCaseAlts ::
     (IsVar a)
  => Exp a -> Env b a -> Fwd (CaseAlt Exp b)
  -> EvalM (Maybe (Eval a))
evalCaseAlts e env = \case
  FwdNil -> return Nothing
  CaseAlt pat body :< alts -> do
    (e', mbRes) <- evalCaseAlt e env pat
    case mbRes of
      Nothing -> evalCaseAlts e' env alts
      Just env' -> Just <$> eval (removeSusp (susp env' body))

evalCaseAlt ::
     (IsVar a)
  => Exp a -> Env b a -> Pattern irr Exp b c
  -> EvalM (Exp a, Maybe (Env c a))
evalCaseAlt e env pat0 = case pat0 of
  PatDefault b -> return (e, Just (envCons b e env))
  PatTyped pat _ty -> evalCaseAlt e env pat
  pat@(PatPrim p) -> do
    ev <- eval (removeSusp e)
    fmap (evalExp ev,) $ case ev of
      EvalCanonical c -> case c of
        Prim p' -> if p == p'
          then return (Just env)
          else return Nothing
        _ -> Left (EEIllTypedCase c pat)
      EvalNeutral{} -> return Nothing
  PatVariant (MkPatVariant lbl pat _impl) -> do
    ev <- eval (removeSusp e)
    case ev of
      EvalCanonical c -> case c of
        Variant (MkVariant lbl' e' impl) -> if lbl == lbl'
          then do
            (ev', mbRes) <- evalCaseAlt e' env pat
            let evE = canonical (Variant (MkVariant lbl' ev' impl))
            case mbRes of
              Nothing ->
                return (evE, Nothing)
              Just env' -> return (evE, Just env')
          else return (evalExp ev, Nothing)
        _ -> Left (EEIllTypedCase c pat0)
      EvalNeutral{} -> return (evalExp ev, Nothing)
  PatRecord patRec -> evalPatRecord e env patRec


evalPatRecord ::
     (IsVar a)
  => Exp a -> Env b a -> PatRecord irr Exp b c
  -> EvalM (Exp a, Maybe (Env c a))
evalPatRecord e env patRec0 = case patRec0 of
  PatRecordNil -> return (e, Just env)
  PatRecordCons lbl pat patRec -> do
    ev <- eval (removeSusp e)
    case ev of
      EvalNeutral{} -> return (evalExp ev, Nothing)
      EvalCanonical c -> case c of
        Record flds -> case LL.lookup flds lbl of
          Nothing -> Left (EERecordFieldMissing lbl e)
          Just fld -> do
            (e', mbRes) <- evalCaseAlt fld env pat
            let evE = canonical (Record (LL.insert flds lbl e'))
            case mbRes of
              Nothing -> return (evE, Nothing)
              Just env1 -> evalPatRecord evE env1 patRec
        _ -> Left (EEIllTypedCase c (PatRecord patRec))

letToLam :: LetArgs Exp from to -> Exp to -> Exp from
letToLam largs0 body = case largs0 of
  LetArgsNil -> body
  LetArgsCons ty pat largs -> canonical (Lam ty pat (letToLam largs body))

evalPrimOp :: PrimOp -> Bwd Prim -> Either EvalError Prim
evalPrimOp pop args = case (pop, toList args) of
  (PrimOpInt64Plus, [PrimInt64 x, PrimInt64 y]) -> return (PrimInt64 (x+y))
  (PrimOpInt64Minus, [PrimInt64 x, PrimInt64 y]) -> return (PrimInt64 (x-y))
  _ -> Left (EEBadPrimOpArgs pop (bwdReverse args))
