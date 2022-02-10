module Eff.Codegen.SourceToRacket where

import Eff.Prelude
import Eff.Source
import Eff.Racket

import Eff.Fresh

data EffContext = MkEffContext {unEffContext :: Map Name EffSig}

compile :: (Members '[Fresh Text Name, State EffContext] r) => [Decl] -> Eff r [RacketExpr]
compile (Def x e : r) = (:) 
    <$> (RDefine x <$> compileExpr (RHash []) e)
    <*> compile r
compile (DefEff l sig : r) = do
    modify (\(MkEffContext m) -> MkEffContext (insert l sig m))
    compile r
compile [] = pure []

compileExpr :: (Members '[Fresh Text Name]) r => RacketExpr -> Expr -> Eff r RacketExpr
compileExpr w (EVal v) = compileVal w v
compileExpr w (App e1 e2) = do
    e1' <- compileExpr w e1
    e2' <- compileExpr w e2
    pure $ RApp e1' [w, e2']

compileExpr w (AppType e _ty) = compileExpr w e -- Types are erased
compileExpr w (Let x _ty e1 e2) = do
    e1' <- compileExpr w e1
    RLet [(x, e1')] . pure <$> compileExpr w e2

compileExpr w (Add e1 e2) = RAdd <$> compileExpr w e1 <*> compileExpr w e2
compileExpr w (LE e1 e2) = RLE <$> compileExpr w e1 <*> compileExpr w e2
compileExpr w (If c th el) = RIf <$> compileExpr w c <*> compileExpr w th <*> compileExpr w el

compileVal :: (Members '[Fresh Text Name]) r => RacketExpr -> Value -> Eff r RacketExpr
compileVal w (Var x) = pure $ RVar x
compileVal w (Lambda _es x _ty e) = do
    w' <- freshVar @Text "w"
    RLambda [w', x] . pure <$> compileExpr (RVar w') e
compileVal w (TyLambda _x _k v) = compileVal w v -- TODO: ?
compileVal w (Handler h) = do
    body' <- freshVar @Text "body"
    m' <- freshVar @Text "m"
    w' <- freshVar @Text "w"
    w'' <- freshVar @Text "w"
    handlerHash <- handlerToHash h
    pure $ RLambda [w', body'] 
         [ RLet [ (m', RMakeContinuationPromptTag m')
                , (w'', setEvidenceFor m' h handlerHash (RVar w'))]
            [ RPromptAt (RVar m') (RApp (RVar body') [RVar w'', RIntLit 0]) -- TODO: (RIntLit 0) should really be a unit argument
            ] 
         ]
        where
            setEvidenceFor m' h@(MkHandler l _) handlerHash w' = RHashSet w' (RSymbol l) (RList [RVar m', handlerHash, w'])
            handlerToHash (MkHandler l ops) = RHash <$> forM ops \(op, (k, v, e)) -> do
                e' <- compileExpr w e
                pure (RSymbol op, RLambda [v, k] [e'])

compileVal _w (Perform l op _epsilon0 _tys) = do
    w' <- freshVar @Text "w"
    v' <- freshVar @Text "v"
    k' <- freshVar @Text "k"
    ev' <- freshVar @Text "ev"
    m' <- freshVar @Text "m"
    h' <- freshVar @Text "h"
    f' <- freshVar @Text "f"
    w'' <- freshVar @Text "w"
    x' <- freshVar @Text "x"
    resumptionArgs' <- freshVar @Text "resumption-args"
    resumptionVal' <- freshVar @Text "resumption-val"
    resumptionW' <- freshVar @Text "resumption-w"
    pure $ RLambda [w', v']
        [   RLet [
                (ev', RHashRef (RVar w') (RSymbol l))
            ,   (m', RCadr 0 (RVar ev'))
            ,   (h', RCadr 1 (RVar ev'))
            ,   (f', RHashRef (RVar h') (RSymbol op))
            ,   (resumptionArgs', RControlAt (RVar m') k' (RApp (RVar f') [RVar v', (RLambda [w'', x'] [RApp (RVar k') [RVar x']])]))
            ,   (resumptionVal', RCadr 0 (RVar resumptionArgs'))
            ,   (resumptionW', RCadr 1 (RVar resumptionArgs'))
            ]
            [ 
                
            ]
        ]
compileVal w (IntLit i) = pure $ RIntLit i

