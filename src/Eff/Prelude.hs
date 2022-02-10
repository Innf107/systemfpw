module Eff.Prelude (
    module Export
,   Name
,   evalState
,   Members
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
    ,   state
    ,   evalState
    ,   execState
    ,   runState

    ,   fromException
    ,   Op
    ,   Type
    )
import Relude.Extra as Export hiding (type (++))

import Cleff as Export hiding (Handler)
import Cleff.State as Export
import Cleff.Reader as Export
import Cleff.Writer as Export
import Cleff.Error as Export

import Control.Lens as Export (makeLenses, ifor, ifor_)

import Eff.Pretty as Export

type Name = Text


defer :: Applicative f => f a -> f b -> f b
defer x y = y <* x

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s = fmap fst . runState s

type Members :: [a] -> [a] -> Constraint
type family Members es1 es2 where
    Members '[] es2 = ()
    Members (e : es1) es2 = (e :> es2, Members es1 es2)