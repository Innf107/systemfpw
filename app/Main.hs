module Main where

import Eff.Prelude
import Eff.Source
import Eff.Racket
import Eff.Stack hiding (Add, LE)
import Eff.Codegen.SourceToStack as Source2Stack
import Eff.Codegen.SourceToRacket as Source2Racket
import Eff.Codegen.StackToASM as Stack2ASM
import Eff.Fresh

-- This works!!!
-- Expected: 10
readerTest :: [Decl]
readerTest = [
        DefEff "Reader" $ MkEffSig [
            ("ask", [], unitT, intT)
        ]
 
    ,   let epsilon = (EffCons "Reader" EffNil) in
        Def "test" (EVal $ Lambda epsilon "x" unitT 
            (Add 
                (App (EVal $ Perform "Reader" "ask" epsilon []) (EVal unitVal)) 
                (App (EVal $ Perform "Reader" "ask" epsilon []) (EVal unitVal))))

    ,   Def "main" (EVal $ Lambda EffNil "u" unitT
        (App (EVal $ Handler (MkHandler "Reader" 
            [
                ("ask", ("k", "u2", (App (EVal $ Var "k") (EVal $ IntLit 5))))
            ])) 
            (EVal $ Var "test")))
    ]

-- This works too!!!
-- Expected: 5
stateTest :: [Decl]
stateTest = let epsilon = (EffCons "State" EffNil) in [
        DefEff "State" $ MkEffSig [
            ("get", [], unitT, intT)
        ,   ("put", [], intT, unitT)
        ]

    ,   Def "inc" (EVal $ Lambda epsilon "_" unitT
            $ Let "x" intT (App (EVal $ Perform "State" "get" epsilon []) (EVal unitVal))
            $ App (EVal $ Perform "State" "put" epsilon []) (Add (EVal $ Var "x") (EVal $ IntLit 1)))

    ,   Def "test" (EVal $ Lambda epsilon "_" unitT 
            $ Let "_" unitT (App (EVal $ Var "inc") (EVal unitVal))
            $ Let "_" unitT (App (EVal $ Var "inc") (EVal unitVal))
            $ Let "_" unitT (App (EVal $ Var "inc") (EVal unitVal))
            $ (EVal $ Lambda epsilon "s" intT (EVal $ Var "s")))

    ,   Def "main" $ EVal $ Lambda EffNil "_" unitT
        $ App
            (App 
                (EVal $ Handler (MkHandler "State" [
                    ("get", ("k", "x", EVal 
                        $ Lambda epsilon "y" intT 
                            (App 
                                (App (EVal $ Var "k") (EVal $ Var "y"))
                                (EVal $ Var "y"))))
                ,   ("put", ("k", "x", EVal
                        $ Lambda epsilon "y" intT
                            (App 
                                (App
                                    (EVal $ Var "k")
                                    (EVal unitVal))
                                (EVal $ Var "x")
                                )))
                ]))
                (EVal $ Var "test"))
        (EVal $ IntLit 2)
    ]

unitVal :: Value
unitVal = IntLit 0 -- well... let's not worry about this for now


-- Only works with reentrant (multi-shot) continuations
nonDetTest :: [Decl]
nonDetTest = let epsilon = EffCons "NonDet" EffNil in [
        DefEff "NonDet" (MkEffSig 
            [("flip", [], unitT, boolT)])

    ,   Def "main" $ 
        (App (EVal $ Handler (MkHandler "NonDet" [
            ("flip", ("k", "x", undefined)) -- weeelll, lists are not a thing yet... let's not worry about it for now.
        ])) undefined)
    ]

-- This also works!
-- Expected: 3
overrideTest :: [Decl]
overrideTest = [
        DefEff "Reader" $ MkEffSig [
            ("ask", [], unitT, intT)
        ]
    
    ,   Def "test" $ EVal $ Lambda epsilon "_" unitT 
        $ Let "x" intT perfAsk
        $ Add 
            (EVal (Var "x")) 
            (App (EVal $ Handler hreader1) 
                (EVal $ Lambda epsilon "_" unitT perfAsk))
    
    ,   Def "main" $ EVal $ Lambda epsilon "_" unitT
        $ App (EVal $ Handler hreader2) (EVal $ Var "test")
    ]
    where 
        epsilon = EffCons "Reader" EffNil

        hreader1 = MkHandler "Reader" [
                ("ask", ("k", "_", App (EVal $ Var "k") (EVal (IntLit 1))))
            ]
        hreader2 = MkHandler "Reader" [
                ("ask", ("k", "_", App (EVal $ Var "k") (EVal (IntLit 2))))
            ]

        perfAsk = App (EVal $ Perform "Reader" "ask" epsilon []) (EVal unitVal)

-- very relevant for tail-resumptive optimizations
-- This Works!
-- Expected: 2
evilTest :: [Decl]
evilTest = [
        DefEff "evil" $ MkEffSig [
            ("evil", [], unitT, unitT)
        ]

    ,   DefEff "reader" $ MkEffSig [
            ("ask", [], unitT, intT)
        ]

    ,   Def "f" $ EVal $ Lambda EffNil "k" todoT $ 
            (EVal $ Handler hread2)
            `App` 
            (EVal $ Lambda EffNil "_" unitT ((EVal $ Var "k") `App` (EVal unitVal)))

    ,   Def "main" $ EVal 
            $ Lambda EffNil "_" unitT 
            $ (EVal $ Var "f") 
            `App` (EVal (Handler hread) 
                `App` (EVal $ Lambda epsilon "_" unitT 
                    (EVal (Handler hevil) 
                        `App` (EVal $ Lambda epsilon "_" unitT 
                        $ Let "_" intT perfAsk
                        $ Let "_" unitT perfEvil
                        $ perfAsk))))
    ]
    where
        epsilon = EffCons "evil" (EffCons "reader" EffNil)
        hevil = MkHandler "evil" [
                ("evil", ("k", "x", EVal $ Var "k"))
            ]
        hread = MkHandler "reader" [
                ("ask", ("k", "x", App (EVal $ Var "k") (EVal $ IntLit 1)))
            ]
        hread2 = MkHandler "reader" [
                ("ask", ("k", "x", App (EVal $ Var "k") (EVal $ IntLit 2)))
            ]
        perfAsk  = App (EVal $ Perform "reader" "ask"  epsilon []) (EVal unitVal)
        perfEvil = App (EVal $ Perform "evil"   "evil" epsilon []) (EVal unitVal)

todoT :: Type
todoT = TyCon "TODO" KType []

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
boolT :: Type
boolT = TyCon "Bool" KType []

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
    let racket = runPure
            $ runFreshName
            $ evalState (MkEffContext mempty)
            $ Source2Racket.compile decls
    putTextLn "\n<<RACKET>>"
    putTextLn (prettyRacketProgram racket)
    
    writeFileText "out.rkt" (prettyRacketProgram racket)

    pure ()

main :: IO ()
main = runTest evilTest
