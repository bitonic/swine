module Swine.Surface.Pretty where

import           Swine.Pretty
import           Swine.Surface.Exp
import           Swine.Prelude
import           Swine.Prim
import           Swine.Meta

data Position
  = PosArg
  | PosNormal

parensIfArg :: Position -> Doc -> Doc
parensIfArg = \case
  PosArg -> parens
  PosNormal -> id

prettyBinder :: Binder -> Doc
prettyBinder = \case
  Bind n -> text n
  Ignore n -> "_" <> text n

prettyTypedPattern :: Pattern -> Option Type -> Doc
prettyTypedPattern pat = \case
  None -> prettyPattern pat
  Some ty -> parens (prettyPattern pat <+> ":" <+> prettyExp PosNormal ty)

prettyExp :: Position -> Exp -> Doc
prettyExp pos = \case
  Type -> "Type"
  e@LamType{} -> prettyLamType pos e
  RecordType flds -> group $
    hangNoGroup (
      "{" <##>
      (vsep (do
        Pair lbl fldTy <- toList flds
        return (hang (text lbl <+> ":" <#> prettyExp PosNormal fldTy <> ";"))))) <##>
    "}"
  VariantType vars -> group $
    hangNoGroup (
      "[" <##>
      (vsep (do
        Pair lbl mbVarTy <- toList vars
        return $ case mbVarTy of
          None -> text lbl <> ";"
          Some varTy ->
            hang (text lbl <+> ":" <#> prettyExp PosNormal varTy <> ";")))) <##>
    "]"
  PrimType pty -> prettyPrimType pty
  Record flds -> group $
    hangNoGroup (
      "{" <##>
      (vsep (do
        Pair lbl e <- toList flds
        return (hang (text lbl <+> "=" <#> prettyExp PosNormal e <> ";"))))) <##>
    "}"
  Variant lbl mbE -> case mbE of
    None -> "[" <> text lbl <> "]"
    Some e -> group $
      hangNoGroup (
        "[" <##>
        hang (text lbl <+> "=" <#> prettyExp PosNormal e)) <##>
      "]"
  Prim p -> prettyPrim pos p
  Var v -> text v
  e@Proj{} -> prettyProj e
  e@App{} -> prettyApp pos e
  PrimOp pop args -> parensIfArg pos $
    hang (vsep (prettyPrimOp pop : map (prettyExp PosArg) (toList args)))
  Hole mbMeta -> case mbMeta of
    None -> "?"
    Some (Meta mv) -> "?" <> int mv
  Case e alts -> group $
    hangNoGroup (
      "case" <+> prettyExp PosArg e <+> "{" <##>
      vsep (do
        Pair pat body <- toList alts
        return (hang (prettyPattern pat <+> "->" <#> prettyExp PosNormal body <> ";")))) <##>
    "}"
  Let n pars0 mbResTy body e -> parensIfArg pos $ vsep
    [ hang $ vsep $
        [ "let" <+> prettyBinder n
        , hang (vsep (map (\(Pair pat mbTy) -> prettyTypedPattern pat mbTy) (toList pars0)))
        ] <>
        (case mbResTy of
          None -> []
          Some resTy -> [hang (":" <#> prettyExp PosNormal resTy)]) <>
        [ hang ("=" <#> (prettyExp PosNormal body <> ";"))
        ]
    , prettyExp PosNormal e
    ]
  e@Lam{} -> prettyLam pos e
  PropType p -> group (hangNoGroup ("|" <##> prettyProp PosNormal p) <##> "|")
  Coe ty1 ty2 eq val -> group $ vsep
    [ "coe"
    , prettyExp PosArg ty1
    , prettyExp PosArg ty2
    , prettyExp PosArg eq
    , prettyExp PosArg val
    ]
  Axiom prop -> group $ vsep
    [ "axiom"
    , prettyProp PosArg prop
    ]

prettyPrimType :: PrimType -> Doc
prettyPrimType = \case
  PrimTypeInt64 -> "I64"

prettyPrim :: Position -> Prim -> Doc
prettyPrim pos p0 = parensIfArg pos $ case p0 of
  PrimInt64 n -> "i64" <+> integer (fromIntegral n)

prettyPrimOp :: PrimOp -> Doc
prettyPrimOp = \case
  PrimOpInt64Plus -> "I64+"
  PrimOpInt64Minus -> "I64-"

prettyLamType :: Position -> Type -> Doc
prettyLamType pos ty0 = parensIfArg pos (go0 BwdNil ty0)
  where
    go0 prevArgs = \case
      LamType mbN argTy rest -> go0 (prevArgs :> (mbN, argTy)) rest
      ty -> let
        go :: [(Option Binder, Type)] -> [Doc]
        go = \case
          [] -> []
          (mbN, argTy) : args -> let
            doc = case mbN of
              None -> prettyExp PosArg argTy
              Some n -> parens (prettyBinder n <> ":" <+> prettyExp PosNormal argTy)
            arr = case (mbN, args) of
              (Some _, ((Some _, _) : _)) -> ""
              _ -> " -> "
            in (doc <> arr) : go args
        in hang (vcat (go (toList prevArgs) <> [prettyExp PosNormal ty]))

prettyProj :: Exp -> Doc
prettyProj = go BwdNil
  where
    go prevProjs = \case
      Proj e lbl -> go (prevProjs :> lbl) e
      e -> hang (vcat (prettyExp PosArg e : ["." <> text lbl | lbl <- toList prevProjs]))

prettyApp :: Position -> Exp -> Doc
prettyApp pos = parensIfArg pos . go []
  where
    go :: [Exp] -> Exp -> Doc
    go prevArgs = \case
      App fun arg -> go (arg : prevArgs) fun
      e -> hang (vsep (prettyExp PosArg e : map (prettyExp PosArg) prevArgs))

prettyPattern :: Pattern -> Doc
prettyPattern = \case
  PatBinder b -> prettyBinder b
  PatVariant lbl mbPat -> case mbPat of
    None -> "[" <> text lbl <> "]"
    Some pat -> group $
      hangNoGroup (
        "[" <##>
        (text lbl <+> "=" <#> prettyPattern pat)) <##>
      "]"
  PatRecord flds -> group $
    hangNoGroup (
      "{" <##>
      (vsep (do
        Pair lbl mbPat <- toList flds
        return $ case mbPat of
          None -> text lbl <> ";"
          Some pat -> hang (text lbl <+> "=" <#> prettyPattern pat <> ";")))) <##>
      "}"

prettyLam :: Position -> Exp -> Doc
prettyLam pos = parensIfArg pos . go BwdNil
  where
    go prevArgs = \case
      Lam pat mbTy body -> go (prevArgs :> (pat, mbTy)) body
      e -> group $ vsep
        [ hang ("\\" <+> vsep (map (uncurry prettyTypedPattern) (toList prevArgs)))
        , "->" <+> prettyExp PosNormal e
        ]

prettyProp :: Position -> Prop -> Doc
prettyProp pos = \case
  PropEmpty -> "Empty"
  PropProduct pp -> group $
    hangNoGroup (
      "{" <##>
      (vsep (do
        Pair lbl p <- toList pp
        return (hang (text lbl <+> ":" <#> prettyProp PosNormal p <> ";"))))) <##>
    "}"
  e@PropForall{} -> prettyForall pos e
  PropTypeEq ty1 ty2 -> hang $ vsep
    [ "TyEq"
    , prettyExp PosArg ty1
    , prettyExp PosArg ty2
    ]
  PropValEq ty1 e1 ty2 e2 -> hang $ vsep
    [ "ValEq"
    , prettyExp PosArg e1
    , prettyExp PosArg ty1
    , prettyExp PosArg e2
    , prettyExp PosArg ty2
    ]

prettyForall :: Position -> Prop -> Doc
prettyForall pos ty0 = parensIfArg pos (go0 BwdNil ty0)
  where
    go0 prevArgs = \case
      PropForall mbN argTy rest -> go0 (prevArgs :> (mbN, argTy)) rest
      prop -> let
        go :: [(Option Binder, Type)] -> [Doc]
        go = \case
          [] -> []
          (mbN, argTy) : args -> let
            doc = case mbN of
              None -> prettyExp PosArg argTy
              Some n -> parens (prettyBinder n <> ":" <+> prettyExp PosNormal argTy)
            arr = case (mbN, args) of
              (Some _, ((Some _, _) : _)) -> ""
              _ -> " -> "
            in (doc <> arr) : go args
        in hang (vcat (go (toList prevArgs) <> [prettyProp PosNormal prop]))

