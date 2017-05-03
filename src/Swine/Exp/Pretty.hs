{-# OPTIONS_GHC -fno-warn-orphans #-}
module Swine.Exp.Pretty where

import qualified Data.HashMap.Strict as HMS

import           Swine.Prelude
import           Swine.Pretty
import qualified Swine.Exp as E

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

class Pretty1 f where
  pretty1 :: VarNames a -> f a -> Doc

instance Pretty E.Binder where
  pretty (E.Binder v) = text v

instance Pretty E.Pattern where
  pretty = \case
    E.PatBind v -> pretty v
    E.PatIgnore v -> "_" <> pretty v

prettyExp :: VarNames a -> E.Exp a -> Doc
prettyExp env = \case
  E.Evaluated (E.Canonical (E.Lam pat0 body)) -> let
    (pat, env') = weakenEnv env pat0
    in hang $ vsep
      [ "\\" <> pretty pat <+> "->"
      , prettyExp env' body
      ]
  E.Evaluated (E.Canonical (E.Prim p)) -> pretty p
  e@(E.Evaluated E.Neutral{}) -> fallback e
  e@E.App{} -> fallback e
  E.Let pat0 e1 e2 -> let
    (pat, env') = weakenEnv env pat0
    in vsep
      [ hang $ vsep
          [ "let" <+> pretty pat <+> "<-"
          , prettyExp env e1 <> ";"
          ]
      , prettyExp env' e2
      ]
  E.Susp eenv e -> prettySusp env eenv e
  where
    fallback e = let
      (head, args) = unravelApps e []
      in hang (vsep (map (prettyArg env) (head : args)))

    unravelApps e0 prevArgs = case e0 of
      E.App fun arg -> unravelApps fun (arg : prevArgs)
      E.Evaluated (E.Neutral head args) -> (E.var head, toList args <> prevArgs)
      e -> (e, prevArgs)

prettyArg :: VarNames a -> E.Exp a -> Doc
prettyArg env = \case
  e@(E.Evaluated (E.Canonical (E.Lam _ _))) -> fallback e
  E.Evaluated (E.Neutral v BwdNil) -> prettyVar env v
  e@(E.Evaluated (E.Neutral _ (_ :> _))) -> fallback e
  e@(E.Evaluated (E.Canonical E.Prim{})) -> fallback e
  e@E.App{} -> fallback e
  e@E.Let{} -> fallback e
  e@E.Susp{} -> fallback e
  where
    fallback e = parens (prettyExp env e)

prettyEnvExp :: VarNames b -> E.Env a b -> (Doc, VarNames a)
prettyEnvExp env = \case
  E.EnvComp eenv1 eenv2 -> let
    (eenv2Doc, env') = prettyEnvArg env eenv2
    (eenv1Doc, env'') = prettyEnvArg env' eenv1
    in (hang (vsep ["$comp", eenv1Doc, eenv2Doc]), env'')
  E.EnvWeaken eenv' wk -> let
    (wkDoc, env') = prettyWeaken env wk
    (eenv'Doc, env'') = prettyEnvArg env' eenv'
    in (hang (vsep ["$weaken", eenv'Doc, wkDoc]), env'')
  E.EnvNormal (E.EnvNil wk) -> let
    (wkDoc, eenv') = prettyWeaken env wk
    in ("$nil" <+> wkDoc, eenv')
  E.EnvNormal (E.EnvCons pat0 e eenv') -> let
    (eenv'Doc, env') = prettyEnvExp env eenv'
    (pat, env'') = weakenEnv env' pat0
    in
      ( group $
          hang (parens (pretty pat <+> ":=" <#> prettyExp env e)) <+> "::" <#>
          eenv'Doc
      , env''
      )

prettyEnvArg :: VarNames b -> E.Env a b -> (Doc, VarNames a)
prettyEnvArg vn env = let
  (envDoc, vn') = prettyEnvExp vn env
  in (parens envDoc, vn')

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

prettySusp :: VarNames b -> E.Env a b -> E.Exp a -> Doc
prettySusp vn env e = let
  (envDoc, vn') = prettyEnvArg vn env
  in hang (vsep ["$susp", hang envDoc, prettyArg vn' e])

prettyVar :: VarNames a -> a -> Doc
prettyVar vn v = case lookupEnv vn v of
  Nothing -> error "prettyVar TODO this should be an Either monad"
  Just txt -> text txt

instance Pretty E.Prim where
  pretty = \case
    E.PrimInt64 i -> "i64" <+> pretty (toInteger i)
