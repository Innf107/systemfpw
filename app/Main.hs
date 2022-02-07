module Main where

import Eff.Prelude
import Eff.Source
import Eff.Racket
import Eff.Stack hiding (Add, LE)
import Eff.Codegen.SourceToStack as Source2Stack
import Eff.Codegen.SourceToRacket as Source2Racket
import Eff.Codegen.StackToASM as Stack2ASM
import Eff.Fresh

effTest :: [Decl]
effTest = [
        Def "test" (EVal $ Lambda EffNil "u" unitT (
            Let "x" intT (undefined {-Perform 0 (EVal $ IntLit 0)-}) 
            $ Let "y" intT (undefined {-Perform 1 (Add (EVal $ Var "x") (EVal $ IntLit 1))-})
            $ undefined -- Perform 0 (EVal $ IntLit 0)
        ))

    ,   Def "runEff" (EVal $ Lambda {- TODO -} ["initial", "action"] (
            Let "state" (EVal $ Var "initial") 
            undefined
            {- (HandleEff 2 (Lambda [] 
                (HandleEff 1 (Lambda [] 
                    (HandleEff 0 (Var "action") []
                        -- get
                        "_get" (Continue (Var "state")))) []
                    -- put
                    "newState" (Seq (UnsafeSet "state" (Var "newState")) (Continue (IntLit 0))))) []
                -- throw
                "throwResult" (Var "throwResult") -- Aborts the continuation
            )-}))
    
    ,   Def "main" (EVal $ Lambda EffNil "u1" unitT 
            (App (Var "runEff") [EVal $ IntLit 5, EVal $ Var "test"]))
    ]

{-
continueTest :: [Decl]
continueTest = [
        Def "test" (EVal $ Lambda EffNil "u" unitT (
            Let "x" (Perform 0 (IntLit 0))
            $ Add (EVal $ Var "x") (EVal $ Var "x")
        ))

    ,   DefFun "runEff" ["env", "action"] (
            HandleEff 0 (Var "action") [] "_ask" (Continue (Var "env"))
        )
    ]


recTest :: [Decl]
recTest = [
        DefFun "fib" ["x"] (
            If (LE (Var "x") (IntLit 1))
                (IntLit 1)
                (Add
                    (App (Var "fib") [Add (Var "x") (IntLit (-1))])
                    (App (Var "fib") [Add (Var "x") (IntLit (-2))]))
        )
    ,   DefFun "main" [] (App (Var "fib") [IntLit 10])
    ]

callTest :: [Decl]
callTest = [
        DefFun "h" ["alpha", "beta"] (
            Add (Var "alpha") (Var "beta")
        )

    ,   DefFun "g" ["a", "b"] (
            Add (Var "a") (App (Var "h") [Var "b", Var "a"])
        )
    ,   DefFun "f" ["x"] (
            If (LE (Var "x") (IntLit 1))
                (Add (App (Var "g") [Var "x", IntLit 1]) (App (Var "h") [Var "x", Var "x"]))
                (Add (Var "x") (Var "x"))
        )
        

    ,   DefFun "main" [] (App (Var "f") [IntLit 5])
    
    ]
-}
simpleTest :: [Decl]
simpleTest = [

        Def "f" 
            (EVal $ Lambda EffNil "x" intT  
                (EVal $ Lambda EffNil "y" intT (
                    If (LE (EVal (Var "x")) (EVal (Var "y")))
                        (EVal (Var "x"))
                        (EVal (Var "y"))
                    )
                )
            )
    ,   Def "main" 
            (EVal $ Lambda EffNil "y" intT 
                (App
                    (App (EVal $ Var "f") (EVal $ IntLit -1)) 
                    (EVal $ IntLit 234)))
    ]

infRecursion :: [Decl]
infRecursion = [
        Def "main" (EVal $ Lambda EffNil "x" intT (
                App (EVal $ Var "main") (EVal $ IntLit 0)
            )
        )
    ]

unitT :: Type
unitT = TyCon "Unit" KType []
intT :: Type
intT = TyCon "Int" KType []


runTest :: [Decl] -> IO ()
runTest decls = do
    putTextLn "\n<<<SOURCE>>>"
    putTextLn (pretty decls)
    
    {-
    let stack = run $ Source2Stack.compile decls
    putTextLn "\n<<<STACK>>>" -- TODO
    putTextLn (pretty stack)
    
    let asm = Stack2ASM.compile stack
    --putTextLn "\n<<ASM>>"
    --putTextLn (pretty asm)

    writeFileText "out.s" (pretty asm)
    -}
    let racket = run $ runFreshName $ Source2Racket.compile decls
    putTextLn "\n<<RACKET>>"
    putTextLn (prettyRacketProgram racket)
    
    writeFileText "out.rkt" (prettyRacketProgram racket)

    pure ()

main :: IO ()
main = runTest effTest
