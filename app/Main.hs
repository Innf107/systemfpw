module Main where

import Eff.Prelude
import Eff.Source
import Eff.Stack hiding (Add, LE)
import Eff.Codegen.SourceToStack as Source2Stack
import Eff.Codegen.StackToASM as Stack2ASM

effTest :: [Decl]
effTest = [
        DefFun "test" [] (
            Let "x" (Perform 0 (IntLit 0)) 
            $ Seq (Perform 1 (Add (Var "x") (IntLit 1)))
            $ Perform 0 (IntLit 0)
        )

    ,   DefFun "runEff" ["initial", "action"] (
            Let "state" (Var "initial") 
            (HandleEff 2 (Lambda [] 
                (HandleEff 1 (Lambda [] 
                    (HandleEff 0 (Var "action") []
                        -- get
                        "_get" (Continue (Var "state")))) []
                    -- put
                    "newState" (Seq (UnsafeSet "state" (Var "newState")) (Continue (IntLit 0))))) []
                -- throw
                "throwResult" (Var "throwResult") -- Aborts the continuation
            ))
    
    ,   DefFun "main" [] (App (Var "runEff") [IntLit 5, Var "test"])
    ]

continueTest :: [Decl]
continueTest = [
        DefFun "test" [] (
            Let "x" (Perform 0 (IntLit 0))
            $ Add (Var "x") (Var "x")
        )

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

simpleTest :: [Decl]
simpleTest = [

        DefFun "f" ["x", "y"] (
            If (LE (Var "x") (Var "y"))
                (Var "x")
                (Var "y")
            )
    ,   DefFun "main" [] (App (Var "f") [(IntLit -1), (IntLit 234)])
    ]

infRecursion :: [Decl]
infRecursion = [
        DefFun "main" [] (
            App (Var "main") []
        )
    ]


runTest :: [Decl] -> IO ()
runTest decls = do
    putTextLn "\n<<<SOURCE>>>"
    putTextLn (pretty decls)
    
    let stack = run $ Source2Stack.compile decls
    putTextLn "\n<<<STACK>>>" -- TODO
    putTextLn (pretty stack)
    
    let asm = Stack2ASM.compile stack
    --putTextLn "\n<<ASM>>"
    --putTextLn (pretty asm)

    writeFileText "out.s" (pretty asm)
    pure ()

main :: IO ()
main = runTest continueTest
