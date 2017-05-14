module Swine.Lang.Rename where

import qualified Swine.Surface as S
import           Swine.Binder
import           Swine.Lang.Exp
import           Swine.Prelude
import qualified Swine.LookupList as LL

data RenameEnv a where
  RenameEnvNil :: RenameEnv TopLevel
  RenameEnvCons :: Option Text -> RenameEnv a -> RenameEnv (Var a)

data RenameError
  = RENotInScope S.Var

type RenameM a = ReaderT (RenameEnv a) (Either RenameError)

topLevelVar :: TopLevel -> RenameM a a
topLevelVar = error "TODO"

lookupVar :: S.Var -> RenameM a a
lookupVar txt = do
  let
    go :: RenameEnv a -> Maybe a
    go = \case
      RenameEnvNil -> Nothing
      RenameEnvCons None env -> F <$> go env
      RenameEnvCons (Some txt') env -> if txt == txt'
        then Just (B txt)
        else F <$> go env
  env <- ask
  case go env of
    Nothing -> lift (Left (RENotInScope txt))
    Just v -> return v

sealCase :: Fwd (CaseAlt NoSusps a) -> RenameM a (Case NoSusps a TopLevel)
sealCase alts = do
  let
    toVar :: Option Text -> Text
    toVar = \case
      None -> "v"
      Some txt -> txt
    go :: (to -> from) -> Case NoSusps from to -> RenameEnv to -> Case NoSusps from TopLevel
    go inj cs = \case
      RenameEnvNil -> cs
      RenameEnvCons mbTxt env -> go (inj . F) (CaseCons (var (inj (B (toVar mbTxt)))) cs) env
  env <- ask
  return (go id (CaseNil alts) env)

binderToSome :: Binder -> Option Text
binderToSome = \case
  Bind b -> Some b
  Ignore{} -> None

renameExp :: S.Exp -> RenameM a (NoSusps a)
renameExp = \case
  S.Type -> return (canonical Type)
  S.LamType mbBind arg0 res0 -> do
    let bind = case mbBind of
          None -> Ignore ""
          Some b -> b
    arg <- renameExp arg0
    let mbTxt = case bind of
          Bind b -> Some b
          Ignore{} -> None
    res <- withReaderT (RenameEnvCons mbTxt) (renameExp res0)
    return (canonical (LamType bind arg res))
  S.RecordType recTy ->
    canonical . RecordType <$> renameTelescope (LL.toFwd recTy)
  S.VariantType varTy ->
    map (canonical . VariantType) $ for varTy $ \case
      None -> return (canonical (RecordType TelescopeNil))
      Some ty -> renameExp ty
  S.PrimType pty -> return (canonical (PrimType pty))
  S.PropType{} -> error "TODO proptype in renamer"
  S.Lam pat0 body0 -> do
    argTy <- topLevelVar Meta
    renamePat pat0 $ \pat -> do
      body <- renameExp body0
      return (canonical (Lam (var argTy) pat body))
  S.Variant lbl e0 -> do
    (e, hid) <- case e0 of
      Some e -> (,) <$> renameExp e <*> pure False
      None -> return (canonical (Record LL.empty), True)
    return (canonical (Variant (MkVariant lbl e hid)))
  S.Record flds -> canonical . Record <$> for flds renameExp
  S.Prim p -> return (canonical (Prim p))
  S.Var v -> var <$> lookupVar v
  S.App fun arg -> app <$> renameExp fun <*> renameExp arg
  S.Proj e0 lbl -> do
    e <- renameExp e0
    return (proj e lbl)
  S.PrimOp pop args -> primOp pop <$> for args renameExp
  S.Hole -> var <$> topLevelVar Meta
  S.Case e0 alts0 -> do
    e <- renameExp e0
    alts <- for alts0 renameCaseAlt
    cs <- sealCase alts
    return (case_ e cs)
  S.Let b pats0 retTy0 body0 rest0 -> renameLetArgs pats0 $ \largs -> do
    retTy <- case retTy0 of
      None -> var <$> topLevelVar Meta
      Some retTy -> renameExp retTy
    body <- renameExp body0
    let (hid, mbTxt) = case b of
          Bind b' -> (False, Some b')
          Ignore{} -> (True, None)
    rest <- withReaderT (RenameEnvCons mbTxt) (renameExp rest0)
    return (let_ (MkLet b largs retTy hid body rest))
  S.Axiom{} -> error "TODO axiom in rename"
  S.Coe{} -> error "TODO coe in rename"

renamePat :: S.Pattern irr -> (forall to. Pattern irr NoSusps from to -> RenameM to a) -> RenameM from a
renamePat pat00 cont = case pat00 of
  S.PatBinder b -> withReaderT (RenameEnvCons (binderToSome b)) (cont (PatDefault b))
  S.PatTyped pat0 ty0 -> do
    ty <- renameExp ty0
    renamePat pat0 (\pat -> cont (PatTyped pat ty))
  S.PatVariant lbl (Some pat0) ->
    renamePat pat0 (\pat -> cont (PatVariant (MkPatVariant lbl pat False)))
  S.PatVariant lbl None ->
    cont (PatVariant (MkPatVariant lbl (PatRecord PatRecordNil) True))
  S.PatRecord rec0 ->
    renamePatRecord (LL.toFwd rec0) (\rec -> cont (PatRecord rec))
  S.PatPrim p -> cont (PatPrim p)

renamePatRecord ::
     Fwd (Pair Label (S.RecordFieldPattern irr))
  -> (forall to. PatRecord irr NoSusps from to -> RenameM to a)
  -> RenameM from a
renamePatRecord flds0 cont = case flds0 of
  FwdNil -> cont PatRecordNil
  Pair lbl (S.RFPNormal pat0) :< flds ->
    renamePat pat0 $ \pat ->
    renamePatRecord flds $ \flds' ->
    cont (PatRecordCons lbl pat flds')
  Pair lbl (S.RFPPun None) :< flds ->
    withReaderT (RenameEnvCons (Some lbl)) $
      renamePatRecord flds $ \flds' ->
        cont (PatRecordCons lbl (PatDefault (Bind lbl)) flds')
  Pair lbl (S.RFPPun (Some ty0)) :< flds -> do
    ty <- renameExp ty0
    withReaderT (RenameEnvCons (Some lbl)) $
      renamePatRecord flds $ \flds' ->
        cont (PatRecordCons lbl (PatTyped (PatDefault (Bind lbl)) ty) flds')

renameLetArgs ::
     Fwd (S.Pattern 'True)
  -> (forall to. LetArgs NoSusps from to -> RenameM to a) -> RenameM from a
renameLetArgs pats0 cont = case pats0 of
  FwdNil -> cont LetArgsNil
  pat :< pats -> do
    ty <- var <$> topLevelVar Meta
    renamePat pat $ \pat' ->
      renameLetArgs pats $ \pats' ->
        cont (LetArgsCons ty pat' pats')

renameCaseAlt :: Pair (S.Pattern 'False) S.Exp -> RenameM a (CaseAlt NoSusps a)
renameCaseAlt (Pair pat0 e0) =
  renamePat pat0 $ \pat -> do
    e <- renameExp e0
    return (CaseAlt pat e)

renameTelescope :: Fwd (Pair Label S.Type) -> RenameM a (Telescope NoSusps a)
renameTelescope = \case
  FwdNil -> return TelescopeNil
  Pair lbl ty0 :< flds0 -> do
    ty <- renameExp ty0
    withReaderT (RenameEnvCons (Some lbl)) $ do
      flds <- renameTelescope flds0
      return (TelescopeCons lbl ty flds)
