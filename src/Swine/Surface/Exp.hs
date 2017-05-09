module Swine.Surface.Exp where

import           Swine.Prelude
import Swine.Prim
import Swine.Meta

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

data Prop
  = PropEmpty
  | PropProduct (Fwd (Pair Label Prop))
  | PropForall (Option Binder) Type Prop
  | PropTypeEq Type Type
  | PropValEq Type Exp Type Exp

data Exp
  -- Types
  = Type
  | LamType (Option Binder) Type Type
  | RecordType (Fwd (Pair Label Type))
  | VariantType (Fwd (Pair Label (Option Type)))
  | PrimType PrimType
  | PropType Prop
  -- Canonical values
  | Lam Pattern (Option Type) Exp
  | Record (Fwd (Pair Label Exp))
  | Variant Label (Option Exp)
  | Prim Prim
  -- Variables
  | Var Var
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
  | Hole (Option Meta)
  -- Coercion
  | Coe
      Type -- First type
      Type -- Second type
      Type -- Proof of equality
      Exp -- Thing to transport
  | Axiom Prop

