module Eff.Stack where


import Eff.Prelude
import Data.Text qualified as T


data Def = DefProc Name [Instr]

        -- Stack Manipulation
data Instr = Push Int       --  |             | -> | n           | 
           | Drop Int       --  | x*n         | -> |             |
           | Shift Int      --  | x ..[n]..   | -> | x ..[n].. x |
           | Swap           --  | x y         | -> | y x         |
           | Dup            --  | x           | -> | x x         |
           | Set Int        --  | x ..[n].. y | -> | y ..[n]..   |
           | Shove Int Int  --  | x*m y*n     | -> | y*n         |

        -- Control Flow
           | Jump           --  | x           | -> |             | (Jumps to [x])
           | Call           --  | x           | -> | a           | (Jumps to [x], 'a' is the return address)
           | Label Int      --  |             | -> |             |
           | PushLabel Int  --  |             | -> | ptr         | 
           | JumpLabel Int  --  |             | -> |             |
           | JumpLabelZ Int --  | x           | -> |             |

        -- Primops
            | Add           -- | x y            | -> | (x + y)      |
            | LE            -- | x y            | -> | (x <= y)     |

        -- Continuations
            | Prompt Int Int-- |     | |     | -> |      | | p   | Switches to a new stack, represented by `p`. The arguments specify the yield and return address respectively.
            | EndPrompt     -- |     | |     | -> |      | |     | Should be called directly after the return address for `Prompt`
            | EndPrompt1    -- |     | | p x | -> | x    | |     | Should be called directly after the return address for `Prompt`
            | Yield         -- |     | | x p | -> | c x  | |     | Switches to p and pushes x, as well as the current continuation
            | Resume        -- | c x | |     | -> |      | | x   | Resumes the continuation at 'c' with x as the argument

        -- Effect prompt handling
            | GetEffPrompt Int     -- |            | -> | p            |           
            | SetEffPrompt Int     -- | p          | -> |              | Prompt Stack: |       | -> | p0   | where p0 is the previous prompt handler
            | RestoreEffPrompt Int -- |            | -> |              | Prompt Stack: | p0    | -> |      | reinstalls p0

        -- Other
            | ProcAddress Name --  |          | -> | a           | ('a' is the address of the procedure 'Name')
            | Panic Text       --  Crash the program with a message
            deriving (Show, Eq)


instance Pretty Def where
    prettyIndent i (DefProc p is) = "proc " <> p <> ": \n" <> T.intercalate "\n" (map ((indent (i + 1) <>) . prettyIndent (i + 1)) is)

instance Pretty Instr where
    prettyIndent _ (Push n) = "push " <> show n
    prettyIndent _ (Drop n) = "drop " <> show n
    prettyIndent _ (Shift n) = "shift " <> show n
    prettyIndent _ Swap = "swap"
    prettyIndent _ Dup = "dup"
    prettyIndent _ (Set n) = "set " <> show n
    prettyIndent _ (Shove n m) = "shove " <> show n <> " " <> show m
    prettyIndent _ Jump = "jump"
    prettyIndent _ Call = "call"
    prettyIndent _ (Label l) = "#" <> show l
    prettyIndent _ (PushLabel l) = "pushLabel " <> show l
    prettyIndent _ (JumpLabel l) = "jumpLabel " <> show l
    prettyIndent _ (JumpLabelZ l) = "jumpLabelZ " <> show l
    prettyIndent _ Add = "add"
    prettyIndent _ LE = "<="
    prettyIndent _ (Prompt i j) = "prompt " <> show i <> " " <> show j
    prettyIndent _ EndPrompt = "endPrompt"
    prettyIndent _ EndPrompt1 = "endPrompt1"
    prettyIndent _ Yield = "yield"
    prettyIndent _ Resume = "resume"
    prettyIndent _ (GetEffPrompt i) = "getPrompt " <> show i
    prettyIndent _ (SetEffPrompt i) = "setPrompt " <> show i
    prettyIndent _ (RestoreEffPrompt i) = "restorePrompt " <> show i
    prettyIndent _ (ProcAddress x) = "procAddress " <> x
    prettyIndent _ (Panic msg) = "panic " <> show msg
