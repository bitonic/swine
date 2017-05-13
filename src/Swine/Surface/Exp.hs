module Swine.Surface.Exp where

import           Swine.Prelude
import           Swine.Prim
import           Swine.Binder

type Var = Text

type Type = Exp

data RecordFieldPattern
  = RFPNormal Pattern
  | RFPPun (Option Type)

data Pattern
  = PatBinder Binder
  | PatTyped Pattern Type
  | PatVariant Label (Option Pattern)
  | PatRecord (LookupList Label RecordFieldPattern)

data Prop
  = PropEmpty
  | PropProduct (LookupList Label Prop)
  | PropForall (Option Binder) Type Prop
  | PropTypeEq Type Type
  | PropValEq Type Exp Type Exp

data Exp
  -- Types
  = Type
  | LamType (Option Binder) Type Type
  | RecordType (LookupList Label Type)
  | VariantType (LookupList Label (Option Type))
  | PrimType PrimType
  | PropType Prop
  -- Canonical values
  | Lam Pattern Exp
  | Record (LookupList Label Exp)
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
      (Fwd Pattern) -- Parameters to the bound thing
      (Option Type) -- Return type
      Exp -- Body
      Exp -- Rest
  | PrimOp PrimOp (Fwd Exp) -- Always fully saturated
  -- Holes
  | Hole
  -- Coercion
  | Coe
      Type -- First type
      Type -- Second type
      Type -- Proof of equality
      Exp -- Thing to transport
  | Axiom Prop

