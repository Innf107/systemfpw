module Eff.Prelude (
    module Export
,   Name
) where

import Relude as Export hiding (
        Reader
    ,   ask
    ,   asks
    ,   local
    ,   runReader

    ,   pass

    ,   State
    ,   get
    ,   put
    ,   gets
    ,   modify
    ,   modify'
    ,   evalState
    ,   execState
    ,   runState

    ,   fromException
    ,   Op
    ,   Type
    )
import Relude.Extra as Export

import Polysemy as Export
import Polysemy.State as Export
import Polysemy.Reader as Export
import Polysemy.Writer as Export
import Polysemy.Error as Export

import Control.Lens as Export (makeLenses, ifor, ifor_)

import Eff.Pretty as Export

type Name = Text


defer :: Applicative f => f a -> f b -> f b
defer x y = y <* x
