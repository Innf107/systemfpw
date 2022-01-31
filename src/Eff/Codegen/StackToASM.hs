module Eff.Codegen.StackToASM where

import Eff.Prelude

import Eff.Codegen.Eff

import Eff.Stack as S
import Eff.ASMx64 as A

import Data.Map qualified as M

compile :: [Def] -> AsmProgram
compile defs = let (strings, instrs) = run $ runCacheStateCounterOrdMap $ traverse compileDef defs 
               in AsmProgram (instrs <> [AsmData "strings" (map makeStringLit (M.toList strings))])
            where
                  makeStringLit :: (Text, Int) -> Text
                  makeStringLit (lit, sid) = "string" <> show sid <> ": " <> show lit 

compileDef :: Members '[CacheState Text Int] r => Def -> Sem r AsmSection
compileDef (DefProc "main" instrs) = AsmProc "_main" . AsmInstrs . concat <$> traverse compileInstrWithComment instrs
compileDef (DefProc name instrs)   = AsmProc name . AsmInstrs . concat <$> traverse compileInstrWithComment instrs

-- | Like compileInstr, but adds a comment with the Stack instruction
compileInstrWithComment :: Members '[CacheState Text Int] r => Instr -> Sem r [Text]
compileInstrWithComment instr = (("; " <> pretty instr):) <$> compileInstr instr

compileInstr :: Members '[CacheState Text Int] r => Instr -> Sem r [Text]
compileInstr (Push n)   = pure [ "push " <> show n ]
compileInstr (Drop n)   = pure [ "add rsp " <> show (n * 8) ]
compileInstr (Shift n)  = pure [ "push qword [rsp+" <> show (8*n) <> "]" ]
compileInstr Swap       = pure [ "mov rax, [rsp+8]"
                              ,  "pop qword [rsp]"
                              ,  "push rax"
                              ]
compileInstr Dup        = pure [ "push qword [rsp]" ]
compileInstr (Set n)    = pure [ "pop rax"
                              ,  "mov [rsp+" <> show (8*n) <> "], rax"
                              ] 
compileInstr (Shove _ 0) = pure []
compileInstr (Shove n m) = pure $ concat [ flip concatMap [(n - 1),(n - 2)..0] 
                                    \i -> [ "mov rax, [rsp+" <> show (i*8) <> "]" 
                                          , "mov [rsp+" <> show ((i + m)*8) <> "], rax"  
                                          ]
                        ,   [ "add rsp, " <> show (8*m) ]
                        ]
compileInstr Jump       = pure [ "pop rax" 
                              ,  "jmp rax"
                              ]
compileInstr Call       = pure [ "pop rax" 
                               , "call rax"
                               ]
compileInstr (Label n)  = pure [ ".l" <> show n <> ":" ]
compileInstr (PushLabel n) = pure [ "push .l" <> show n ] 
compileInstr (JumpLabel n) = pure [ "jmp .l" <> show n ]
compileInstr (JumpLabelZ n) = pure [ "pop rax" 
                                   , "cmp rax, 0"
                                   , "je .l" <> show n
                                   ]
compileInstr Add        = pure [ "pop rax"
                               , "add [rsp], rax"
                               ]
compileInstr LE         = pure [ "pop rax"
                               , "cmp [rsp], rax"
                               , "mov rcx, 1"
                               , "mov rax, 0"
                               , "cmovle rax, rcx"
                               , "mov [rsp], rax"
                               ]
compileInstr (Prompt yield ret) = pure [ "mov rdi, " <> show yield
                                       , "mov rsi, " <> show ret
                                       , "call __prompt"
                                       ]
compileInstr EndPrompt  = pure [ "call __end_prompt" ]

compileInstr EndPrompt1 = pure [ "pop rbx"
                               , "call __end_prompt"
                               , "push rbx"
                               ]
compileInstr Yield      = pure [ "call __yield"
                               ]
compileInstr Resume     = undefined

compileInstr (GetEffPrompt ix) = undefined
compileInstr (SetEffPrompt ix) = undefined
compileInstr (RestoreEffPrompt ix) = undefined


compileInstr (ProcAddress "main") = pure [ "push _main" ]
compileInstr (ProcAddress x) = pure [ "push " <> x ]
compileInstr (Panic msg) = cached msg <&> \strId -> [ "mov rax, string" <> show strId ]



