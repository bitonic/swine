module Swine.Lang.Eval where

import           Swine.Prelude
import           Swine.Lang.Exp
import           Swine.Lang.RemoveSusp

data EvalError

type EvalM = Either EvalError

eval :: forall a. Syntax Exp a -> EvalM (Eval a)
eval = \case
  Var v -> return (EvalNeutral v mempty)
  Canonical c -> return (EvalCanonical c)
  App fun0 arg -> do
    fun <- eval (removeSusp fun0)
    case fun of
      EvalNeutral h args -> return (EvalNeutral h (args :> ElimApp arg))
      EvalCanonical c0 -> case c0 of
        Lam _ty pat body -> evalIrrPat pat body arg

evalIrrPat :: Pattern 'True Exp from to -> Exp to -> Exp from -> EvalM (Eval from)
evalIrrPat pat0 body arg = case pat0 of
  PatDefault b -> eval (removeSusp (susp (envCons b arg envNil) body))
  PatTyped pat _ty -> evalIrrPat pat body arg
  PatRecord patRec -> evalIrrPatRecord patRec body arg

evalIrrPatRecord :: PatRecord 'True Exp from to -> Exp to -> Exp from -> EvalM (Eval from)
evalIrrPatRecord pat0 body arg = case pat0 of
  PatRecordNil -> eval (removeSusp body)
