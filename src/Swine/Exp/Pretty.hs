{-# OPTIONS_GHC -fno-warn-orphans #-}
module Swine.Exp.Pretty where

import qualified Data.HashMap.Strict as HMS

import           Swine.Prelude
import           Swine.Pretty
import qualified Swine.Exp as E
import qualified Swine.LookupList as LL

data Names a where
  NNil :: (a -> Maybe Text) -> Names a
  NCons :: Text -> Names a -> Names (E.Var a)

data VarNames a = VarNames
  { vnCounters :: HashMap Text Int64
  , vnNames :: Names a
  }

newVarNames :: (a -> Maybe Text) -> VarNames a
newVarNames f = VarNames
  { vnCounters = mempty
  , vnNames = NNil f
  }

weakenEnv :: VarNames a -> E.Pattern -> (E.Pattern, VarNames (E.Var a))
weakenEnv vn pat = let
  E.Binder n = E.patBinder pat -- TODO check if the var is used in the term and have an ignore binder if it is not
  counter = case HMS.lookup n (vnCounters vn) of
    Nothing -> 0
    Just c -> c
  counters = HMS.insert n (counter+1) (vnCounters vn)
  n' = if counter == 0 then n else n <> "_" <> tshow counter
  in
    ( E.PatBind (E.Binder n')
    , vn
        { vnCounters = counters
        , vnNames = NCons n' (vnNames vn)
        }
    )

strengthenEnv :: VarNames (E.Var a) -> VarNames a
strengthenEnv vn = vn
  { vnNames = case vnNames vn of
      NNil _ -> error "strengthenEnv TODO this should be a left monad throughout..."
      NCons _ ns -> ns
  -- TODO consider _removing_ the right counter to free a slot
  }

lookupEnv :: VarNames a -> a -> Maybe Text
lookupEnv vn = go (vnNames vn)
  where
    go :: Names a -> a -> Maybe Text
    go ns0 v = case ns0 of
      NNil f -> f v
      NCons txt ns -> case v of
        E.B _ -> Just txt
        E.F v' -> go ns v'

instance Pretty E.Binder where
  pretty (E.Binder v) = text v

instance Pretty E.Pattern where
  pretty = \case
    E.PatBind v -> pretty v
    E.PatIgnore v -> "_" <> pretty v

data Position
  = PosArg -- Appears as an arg
  | PosNormal -- Appears as a top level thing

parensIfArg :: Position -> Doc -> Doc
parensIfArg = \case
  PosArg -> parens
  PosNormal -> id

prettyExp :: Position -> VarNames a -> E.Exp a -> Doc
prettyExp pos vn = \case
  E.Syntax e -> prettySyntax pos vn e
  E.Susp env e -> prettySusp pos vn env (E.Syntax e)

prettyApp :: Doc -> [Doc] -> Doc
prettyApp head args = hang (vsep (head : args))

prettySyntax :: Position -> VarNames a -> E.Syntax a -> Doc
prettySyntax pos env = \case
  E.Var v -> prettyVar env v
  E.Canonical (E.Prim p) -> prettyPrim pos p
  e@(E.Canonical (E.Lam _ _)) -> parensIfArg pos (hang ("\\" <> prettyLam env (E.Syntax e)))
  E.Canonical (E.Record rec) -> group $
    hangNoGroup (
      "{" <##>
      vsep (do
        (lbl, e) <- LL.toList rec
        return (hang (text lbl <> ":" <#> prettyExp PosNormal env e)))) <##>
    "}"
  E.Let pat0 e1 e2 -> parensIfArg pos $ let
    (pat, env') = weakenEnv env pat0
    in vsep
      [ hang $ vsep
          [ "let" <+> pretty pat <+> "<-"
          , prettyExp PosNormal env e1 <> ";"
          ]
      , prettyExp PosNormal env' e2
      ]
  E.PrimOp pop args -> prettyApp (pretty pop) (map (prettyExp PosArg env) (toList args))
  e@E.App{} -> let
    (head, args) = unravelApps (E.Syntax e) []
    in prettyApp (prettyExp PosArg env head) (map (prettyExp PosArg env) args)
  e@E.Proj{} -> hang (prettyProj (E.Syntax e))
  where
    unravelApps :: E.Exp a -> [E.Exp a] -> (E.Exp a, [E.Exp a])
    unravelApps e0 prevArgs = case e0 of
      E.Syntax (E.App fun arg) -> unravelApps fun (arg : prevArgs)
      e -> (e, prevArgs)

    prettyLam :: VarNames a -> E.Exp a -> Doc
    prettyLam vn = \case
      E.Syntax (E.Canonical (E.Lam pat0 body)) -> let
        (pat, vn') = weakenEnv vn pat0
        in pretty pat <+> prettyLam vn' body
      e -> "->" <#> prettyExp PosNormal vn e

    prettyProj = \case
      E.Syntax (E.Proj e lbl) -> prettyProj e <##> "." <> text lbl
      e -> prettyExp PosArg env e

prettyEnv :: Position -> VarNames b -> E.Env a b -> (Doc, VarNames a)
prettyEnv pos env = \case
  E.EnvComp eenv1 eenv2 -> let
    (eenv2Doc, env') = prettyEnv PosArg env eenv2
    (eenv1Doc, env'') = prettyEnv PosArg env' eenv1
    in (parensIfArg pos (hang (vsep ["$comp", eenv1Doc, eenv2Doc])), env'')
  E.EnvWeaken eenv' wk -> let
    (wkDoc, env') = prettyWeaken env wk
    (eenv'Doc, env'') = prettyEnv PosArg env' eenv'
    in (parensIfArg pos (hang (vsep ["$weaken", eenv'Doc, wkDoc])), env'')
  E.EnvNormal (E.EnvNil wk) -> let
    (wkDoc, eenv') = prettyWeaken env wk
    in (parensIfArg pos ("$nil" <+> wkDoc), eenv')
  E.EnvNormal (E.EnvCons pat0 e eenv') -> let
    (eenv'Doc, env') = prettyEnv PosNormal env eenv'
    (pat, env'') = weakenEnv env' pat0
    in
      ( parensIfArg pos $ group $
          hang (parens (pretty pat <+> ":=" <#> prettyExp PosNormal env e)) <+> "::" <#>
          eenv'Doc
      , env''
      )

prettyWeaken :: VarNames b -> E.Weaken a b -> (Doc, VarNames a)
prettyWeaken vn0 wk0 = let
  (wkNum, vn) = go vn0 wk0
  in (integer wkNum, vn)
  where
    go :: VarNames b -> E.Weaken a b -> (Integer, VarNames a)
    go eenv = \case
      E.WeakenZero -> (0, eenv)
      E.WeakenSucc wk -> let
        (c, eenv') = go (strengthenEnv eenv) wk
        in (c+1, eenv')

prettySusp :: Position -> VarNames b -> E.Env a b -> E.Exp a -> Doc
prettySusp pos vn env e = let
  (envDoc, vn') = prettyEnv PosArg vn env
  in parensIfArg pos (hang (vsep ["$susp", hang envDoc, prettyExp PosArg vn' e]))

prettyVar :: VarNames a -> a -> Doc
prettyVar vn v = case lookupEnv vn v of
  Nothing -> error "prettyVar TODO this should be an Either monad"
  Just txt -> text txt

prettyPrim :: Position -> E.Prim -> Doc
prettyPrim pos p = parensIfArg pos $ case p of
  E.PrimInt64 i -> "i64" <+> pretty (toInteger i)

instance Pretty E.PrimOp where
  pretty = \case
    E.PrimOpPlusInt64 -> "+i64"
