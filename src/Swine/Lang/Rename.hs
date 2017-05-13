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

type RenameM a = ReaderT (RenameEnv a) (Either RenameError)

topLevelVar :: TopLevel -> RenameM a a
topLevelVar = error "TODO"

renameExp :: S.Exp -> RenameM a (NoSusps a)
renameExp = \case
  S.Type -> return (canonical Type)
  S.LamType mbBind arg0 res0 -> do
    let bind = case mbBind of
          None -> Ignore ""
          Some b -> b
    let mbTxt = case bind of
          Bind b -> Some b
          Ignore{} -> None
    arg <- renameExp arg0
    res <- withReaderT (RenameEnvCons mbTxt) (renameExp res0)
    return (canonical (LamType bind arg res))
  S.RecordType recTy -> do
    let
      go :: Fwd (Pair Label S.Type) -> RenameM a (Telescope NoSusps a)
      go = error "TODO"
    canonical . RecordType <$> go (LL.toFwd recTy)
  S.VariantType varTy ->
    map (canonical . VariantType) $ for varTy $ \case
      None -> return (canonical (RecordType TelescopeNil))
      Some ty -> renameExp ty
  S.PrimType pty -> return (canonical (PrimType pty))
  S.PropType{} -> error "TODO proptype in renamer"
