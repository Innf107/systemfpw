module Eff.Codegen.SourceToRacket where

import Eff.Prelude
import Eff.Source
import Eff.Racket

import Eff.Fresh

compile :: (Members '[Fresh Text Name]) r => [Decl] -> Sem r [RacketExpr]
compile (Def x e : r) = (:) <$> 
    (do
        evecName <- freshVar "w"
        e' <- compileExpr (RVar evecName) e
        pure $ RDefineF x [] [
                (RDefine evecName RHash)
            ,   e'
            ]
        )
    <*> compile r
compile (DefEff _ _ : r) = undefined
compile [] = pure []

compileExpr :: (Members '[Fresh Text Name]) r => RacketExpr -> Expr -> Sem r RacketExpr
compileExpr w (EVal v) = compileVal w v
compileExpr w (App e1 e2) = RApp 
                        <$> compileExpr w e1 
                        <*> (pure <$> compileExpr w e2)
compileExpr w (AppType e _ty) = compileExpr w e -- Types are erased
compileExpr w (Let x _ty e1 e2) = do
    e1' <- compileExpr w e1
    RLet [(x, e1')] . pure <$> compileExpr w e2

compileExpr w (Add e1 e2) = RAdd <$> compileExpr w e1 <*> compileExpr w e2
compileExpr w (LE e1 e2) = RLE <$> compileExpr w e1 <*> compileExpr w e2
compileExpr w (If c th el) = RIf <$> compileExpr w c <*> compileExpr w th <*> compileExpr w el

compileVal :: (Members '[Fresh Text Name]) r => RacketExpr -> Value -> Sem r RacketExpr
compileVal w (Var x) = pure $ RVar x
compileVal w (Lambda _es x _ty e) = do
    w' <- freshVar "w"
    RLambda [w', x] . pure <$> compileExpr (RVar w') e
compileVal w (TyLambda _x _k v) = compileVal w v -- TODO: ?
compileVal w (Handler h) = do
    body' <- freshVar "body"
    m' <- freshVar "m"
    w' <- freshVar "w"
    pure $ RLambda [body'] 
         $ RLet [ (m', RMakeContinuationPromptTag m')
                , (w', RHashSet w undefined undefined)]
         $ RPromptAt (Var m') (RApp body' [RVar w', IntLit 0]) -- TODO: (IntLit 0) should be a unit argument?
compileVal w (Perform op _e _tys) = undefined 
compileVal w (IntLit i) = pure $ RIntLit i

