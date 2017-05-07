{-# OPTIONS_GHC -fno-warn-orphans #-}
module Swine.Pretty
  ( module X
  , hang
  , hangNoGroup
  , text
  , render
  , indent
  , (<#>)
  , (<##>)
  ) where

import           Text.PrettyPrint.ANSI.Leijen as X hiding (hang, (<>), text, indent, (<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Text as T
import           Text.Trifecta () -- For the semigroup instance

import           Swine.Prelude

text :: Text -> Doc
text = PP.text . T.unpack

hang :: Doc -> Doc
hang = group . hangNoGroup

hangNoGroup :: Doc -> Doc
hangNoGroup = nest 2

indent :: Doc -> Doc
indent = PP.indent 2

render :: (IsString s) => Doc -> s
render x = fromString (displayS (renderPretty 0.8 80 x) "")

(<#>) :: Doc -> Doc -> Doc
(<#>) = (PP.<$>)

(<##>) :: Doc -> Doc -> Doc
(<##>) = (PP.<$$>)

