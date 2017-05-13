module Swine.Binder where

import           Swine.Prelude

type Label = Text

data Binder
  = Bind Text
  | Ignore Text

-- Useful when we need generated names
binderVar :: Binder -> Text
binderVar = \case
  Bind v -> v
  Ignore v -> "v_" <> v

-- newtype Meta = MkMeta Int

data TopLevel
  = Meta
