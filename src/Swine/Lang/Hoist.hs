module Swine.Lang.Hoist where

import           Swine.Prelude
import           Swine.Lang.Exp
import           Swine.Binder

hoistSyntax :: (forall a. f a -> g a) -> Syntax f b -> Syntax g b
hoistSyntax f = \case
  Canonical c -> Canonical (hoistCanonical f c)
  Var v -> Var v
  App fun arg -> App (f fun) (f arg)
  Proj e lbl -> Proj (f e) lbl
  Case e cs -> Case (f e) (hoistCase f cs)
  Let (MkLet b args retTy retTyHid body rest) ->
    Let (MkLet b (hoistLetArgs f args) (f retTy) retTyHid (f body) (f rest))
  PrimOp pop args -> PrimOp pop (map f args)

hoistCanonical :: (forall a. f a -> g a) -> Canonical f b -> Canonical g b
hoistCanonical f = \case
  Type -> Type
  LamType b arg res -> LamType b (f arg) (f res)
  RecordType tel -> RecordType (hoistTelescope f tel)
  VariantType tys -> VariantType (map f tys)
  PrimType pt -> PrimType pt
  Lam ty pat body -> Lam (f ty) (hoistPattern f pat) (f body)
  Record flds -> Record (map f flds)
  Variant (MkVariant lbl body impl) -> Variant (MkVariant lbl (f body) impl)
  Prim p -> Prim p

hoistTelescope :: (forall a. f a -> g a) -> Telescope f b -> Telescope g b
hoistTelescope f = \case
  TelescopeNil -> TelescopeNil
  TelescopeCons lbl e tel -> TelescopeCons lbl (f e) (hoistTelescope f tel)

hoistPattern :: (forall a. f a -> g a) -> Pattern irr f from to -> Pattern irr g from to
hoistPattern f = \case
  PatDefault b -> PatDefault b
  PatTyped pat ty -> PatTyped (hoistPattern f pat) (f ty)
  PatPrim p -> PatPrim p
  PatVariant (MkPatVariant lbl pat impl) -> PatVariant (MkPatVariant lbl (hoistPattern f pat) impl)
  PatRecord rec -> PatRecord (hoistPatRecord f rec)

hoistPatRecord :: (forall a. f a -> g a) -> PatRecord irr f from to -> PatRecord irr g from to
hoistPatRecord f = \case
  PatRecordNil -> PatRecordNil
  PatRecordCons lbl pat patRec -> PatRecordCons lbl (hoistPattern f pat) (hoistPatRecord f patRec)

hoistCase :: (forall a. f a -> g a) -> Case f from to -> Case g from to
hoistCase f = \case
  CaseNil alts -> CaseNil (map (hoistCaseAlt f) alts)
  CaseCons arg cs -> CaseCons (f arg) (hoistCase f cs)

hoistCaseAlt :: (forall a. f a -> g a) -> CaseAlt f b -> CaseAlt g b
hoistCaseAlt f (CaseAlt pat body) = CaseAlt (hoistPattern f pat) (f body)

hoistLetArgs :: (forall a. f a -> g a) -> LetArgs f from to -> LetArgs g from to
hoistLetArgs f = \case
  LetArgsNil -> LetArgsNil
  LetArgsCons ty pat args -> LetArgsCons (f ty) (hoistPattern f pat) (hoistLetArgs f args)
