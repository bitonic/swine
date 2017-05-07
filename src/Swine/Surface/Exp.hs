module Swine.Surface.Exp where

import           Swine.Prelude
import Swine.Prim

type Var = Text
type Label = Text

data Binder
  = Bind Var
  | Ignore Text

data Pattern
  = PatBinder Binder
  | PatVariant Label (Option Pattern)
  | PatRecord (Fwd (Pair Label (Option Pattern)))

type Type = Exp

data Exp
  -- Types
  = Type
  | LamType (Option Binder) Type Type
  | RecordType (Fwd (Pair Label Type))
  | VariantType (Fwd (Pair Label (Option Type)))
  | PrimType PrimType
  -- Canonical values
  | Lam Pattern (Option Type) Exp
  | Record (Fwd (Pair Label Exp))
  | Variant Label (Option Exp)
  | Prim Prim
  -- Variables
  | Var Var
  -- Annotated
  | Annotated Exp Type
  -- Computation
  | App Exp Exp
  | Proj Exp Label
  -- TODO Add dependent pattern matching
  | Case
      Exp -- The scrutinized
      (Fwd (Pair Pattern Exp))
  | Let
      Binder -- Name of the bound thing
      (Fwd (Pair Pattern (Option Type))) -- Parameters to the bound thing
      (Option Type) -- Return type
      Exp -- Body
      Exp -- Rest
  | PrimOp PrimOp (Fwd Exp) -- Always fully saturated
  -- Holes
  | Hole

