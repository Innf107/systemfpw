module Eff.Codegen.SourceToRacket where

import Eff.Prelude
import Eff.Source
import Eff.Racket

import Eff.Fresh

data EffContext = MkEffContext {unEffContext :: Map Name EffSig}


compile :: (Members '[Fresh Text Name, State EffContext] r) => [Decl] -> Eff r [RacketExpr]
compile (Def x e : r) = (:) 
    <$> (RDefine x <$> compileExpr e)
    <*> compile r
compile (DefEff l sig : r) = do
    modify (\(MkEffContext m) -> MkEffContext (insert l sig m))
    compile r
compile [] = pure []

compileExpr :: (Members '[Fresh Text Name]) r => Expr -> Eff r RacketExpr
compileExpr (EVal v) = compileVal v
compileExpr (App e1 e2) = RApp <$> compileExpr e1 <*> (pure <$> compileExpr e2)

compileExpr (AppType e _ty) = compileExpr e -- Types are erased
compileExpr (Let x _ty e1 e2) = do
    e1' <- compileExpr e1
    RLet [(x, e1')] . pure <$> compileExpr e2

compileExpr (Add e1 e2) = RAdd <$> compileExpr e1 <*> compileExpr e2
compileExpr (LE e1 e2) = RLE <$> compileExpr e1 <*> compileExpr e2
compileExpr (If c th el) = RIf <$> compileExpr c <*> compileExpr th <*> compileExpr el

compileVal :: (Members '[Fresh Text Name]) r => Value -> Eff r RacketExpr
compileVal (Var x) = pure $ RVar x
compileVal (Lambda _es x _ty e) = 
    RLambda [x] . pure <$> compileExpr e

compileVal (TyLambda _x _k v) = compileVal v -- TODO: ?
compileVal (Handler h@(MkHandler l _)) = do
	h' <- handlerToHash h
	pure $ RApp (RVar "handler") [RSymbol l, h']
	where
		handlerToHash (MkHandler l ops) = RHash <$> forM ops \(op, (k, v, e)) -> do
			e' <- compileExpr e
			pure (RSymbol op, RLambda [v, k] [e'])
{-
compileVal (Handler h) = do
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
-}
compileVal (Perform l op _epsilon0 _tys) = pure $ RApp (RVar "perform") [RSymbol l, RSymbol op]
        
compileVal (IntLit i) = pure $ RIntLit i

