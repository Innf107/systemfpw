module Eff.Pretty where

import Relude
import Data.Text qualified as T

class Pretty a where
    pretty :: a -> Text
    pretty = prettyIndent 0
    prettyIndent :: Int -> a -> Text

indent :: Int -> Text
indent i = T.replicate (i *  4) " "

instance Pretty a => Pretty [a] where
    prettyIndent i = T.intercalate "\n\n" . map (prettyIndent i)
