module Swine.Lang.RemoveSusp where

import           Swine.Prelude
import           Swine.Lang.Exp
import           Swine.Binder
import           Swine.Lang.Hoist

-- Removing suspensions
-- --------------------------------------------------------------------

-- TODO add a version that turns it into `let` for better pretty
-- printing. Specifically, single variables stored in the environments
-- should be substituted, but everything else is most likely better
-- in a let. I can come up with heuristics regarding other cases.

removeSusp :: Exp a -> Syntax Exp a
removeSusp = \case
  Syntax e -> e
  Susp env e0 -> case e0 of
    Var v -> removeSusp (envLookup env v)
    Canonical c -> Canonical $ case c of
      Type -> Type
      LamType b arg res -> LamType b (susp env arg) (susp (envAbs b env) res)
      RecordType tel -> RecordType (suspTel env tel)
      VariantType tys -> VariantType (map (susp env) tys)
      PrimType pty -> PrimType pty
      Lam ty pat body -> suspPattern env pat (\env' pat' -> Lam (susp env ty) pat' (susp env' body))
      Record flds -> Record (map (susp env) flds)
      Variant (MkVariant lbl e hid) -> Variant (MkVariant lbl (susp env e) hid)
      Prim p -> Prim p
    App fun arg -> App (susp env fun) (susp env arg)
    Proj e lbl -> Proj (susp env e) lbl
    Case e case_ -> Case (susp env e) (suspCase env case_)
    PrimOp pop args -> PrimOp pop (map (susp env) args)
    Let let_ -> Let (suspLet env let_)

suspTel :: Env a b -> Telescope Exp a -> Telescope Exp b
suspTel env = \case
  TelescopeNil -> TelescopeNil
  TelescopeCons lbl e rest -> TelescopeCons lbl (susp env e) (suspTel (envAbs (Bind lbl) env) rest)

suspCaseAlt :: Env a b -> CaseAlt Exp a -> CaseAlt Exp b
suspCaseAlt env (CaseAlt pat body) =
  suspPattern env pat (\env' pat' -> CaseAlt pat' (susp env' body))

suspPattern ::
     Env from from' -> Pattern irr Exp from to
  -> (forall to'. Env to to' -> Pattern irr Exp from' to' -> a)
  -> a
suspPattern env pat0 cont = case pat0 of
  PatDefault b -> cont (envAbs b env) (PatDefault b)
  PatPrim p -> cont env (PatPrim p)
  PatVariant (MkPatVariant lbl pat impl) ->
    suspPattern env pat (\env' pat' -> cont env' (PatVariant (MkPatVariant lbl pat' impl)))
  PatRecord patRec -> suspPatRecord env patRec (\env' patRec' -> cont env' (PatRecord patRec'))
  PatTyped pat ty -> suspPattern env pat (\env' pat' -> cont env' (PatTyped pat' (susp env ty)))

suspPatRecord ::
     Env from from' -> PatRecord irr Exp from to
  -> (forall to'. Env to to' -> PatRecord irr Exp from' to' -> a)
  -> a
suspPatRecord env pat0 cont = case pat0 of
  PatRecordNil -> cont env PatRecordNil
  PatRecordCons lbl pat patRec ->
    suspPattern env pat $ \env' pat' ->
    suspPatRecord env' patRec $ \env'' patRec' ->
    cont env'' (PatRecordCons lbl pat' patRec')

suspLet :: Env a b -> Let Exp a -> Let Exp b
suspLet env (MkLet b args retTy retTyHidden body rest) = suspLetArgs env args $ \env' args' ->
  MkLet b args' (susp env' retTy) retTyHidden (susp env' body) (susp (envAbs b env) rest)

suspLetArgs ::
     Env from from' -> LetArgs Exp from to
  -> (forall to'. Env to to' -> LetArgs Exp from' to' -> a)
  -> a
suspLetArgs env largs0 cont = case largs0 of
  LetArgsNil -> cont env LetArgsNil
  LetArgsCons ty pat largs ->
    suspPattern env pat $ \env' pat' ->
    suspLetArgs env' largs $ \env'' largs' ->
    cont env'' (LetArgsCons (susp env ty) pat' largs')

suspCase ::
     Env from to -> Case Exp from a -> Case Exp to a
suspCase env = \case
  CaseNil alts -> CaseNil alts
  CaseCons b arg cs -> CaseCons b (susp env arg) (suspCase env cs)

removeAllSusps :: Exp a -> NoSusps a
removeAllSusps = NoSusps . hoistSyntax removeAllSusps . removeSusp

