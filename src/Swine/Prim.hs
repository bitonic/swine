module Swine.Prim where

import Swine.Prelude

data Prim
  = PrimInt64 Int64
  deriving (Eq, Ord, Show, Read)

data PrimType
  = PrimTypeInt64
  deriving (Eq, Ord, Show, Read)

data PrimOp
  = PrimOpInt64Plus
  | PrimOpInt64Minus
  deriving (Eq, Ord, Show, Read)

primOpArity :: PrimOp -> Int
primOpArity = \case
  PrimOpInt64Plus -> 2
  PrimOpInt64Minus -> 2
