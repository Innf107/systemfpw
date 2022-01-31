module Eff.ASMx64 where

import Eff.Prelude

import Data.Text qualified as T

data AsmProgram = AsmProgram [AsmSection] deriving (Show, Eq)

data AsmSection = AsmProc Text AsmInstrs 
                | AsmData Text [Text] 
                deriving (Show, Eq) -- Not yet needed

newtype AsmInstrs = AsmInstrs [Text] deriving (Show, Eq)


instance Pretty AsmProgram where
    prettyIndent i (AsmProgram secs) = T.intercalate ("\n" <> indent i) $ [
            "global _main"
        ,   "extern panic"
        ,   prettyIndent i secs
        ]

instance Pretty AsmSection where
    prettyIndent i (AsmProc name instrs) = name <> ":\n" <> indent (i + 1) <> prettyIndent (i + 1) instrs
    prettyIndent i (AsmData name fields) = name <> ":\n" <> indent (i + 1) <> T.intercalate ("\n" <> indent (i + 1)) fields


instance Pretty AsmInstrs where
    prettyIndent i (AsmInstrs instrs) = T.intercalate ("\n" <> indent i) instrs
