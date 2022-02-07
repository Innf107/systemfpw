{-#LANGUAGE TemplateHaskell#-}
module Eff.Codegen.SourceToStack where

import Eff.Prelude
import Eff.Source as Source
import Eff.Stack as Stack
import Eff.Prelude (undefined)

import Data.Set qualified as S

data CompState = CompState {
    _functions :: Set Name
,   _nextLabel :: Int
}
makeLenses ''CompState

data DeclState = DeclState {
    _baseOffset :: Int
,   _localVarBaseIndices :: Map Name Int
,   _contBaseIndex :: Int
}
makeLenses ''DeclState

compile :: [Decl] -> Sem r [Def]
compile = undefined -- fmap concat . evalState initialCompState . traverse (evalState initialDeclState . compileDecl)
    
{-
initialDeclState :: DeclState
initialDeclState = DeclState {
    _baseOffset = 0
,   _localVarBaseIndices = mempty
,   _contBaseIndex = -1
}
initialCompState :: CompState
initialCompState = CompState {
    _functions = mempty
,   _nextLabel = 0
}

modifyBaseOffset :: Members '[State DeclState] r => (Int -> Int) -> Sem r ()
modifyBaseOffset f = modify (baseOffset %~ f)


compileDecl :: Members '[State DeclState, State CompState] r => Decl -> Sem r [Def]
compileDecl (DefFun f xs e) = do
    modify (functions %~ S.insert f)
    modify (baseOffset .~ length xs + 1)
    ifor_ xs \i x -> modify (localVarBaseIndices %~ insert x i)
    (eInstrs, (offsets, (defs, _))) <- runWriterAssocR @[Instr] $ runWriterAssocR  @[(Instr, Int)] $ runWriterAssocR @[Def] $ compileExpr e

    traceM $ toString $ unlines (map (\(i, o) -> show o <> ":  " <> pretty i) offsets)
    pure $ DefProc f (eInstrs <> [Shove 2 (length xs), Swap, Jump]) : defs


compileExpr :: Members '[State DeclState, State CompState, Writer [Instr], Writer [Def], Writer [(Instr, Int)]] r 
            => Expr 
            -> Sem r ()
compileExpr (IntLit n) = emit $ Push n
compileExpr (Var x) = do
    gets (lookup x . _functions) >>= \case
        Nothing -> emit . Shift =<< getVarOffset x
        Just _ -> emit $ ProcAddress x
compileExpr (App f xs) = do
    compileExpr f
    traverse compileExpr xs
    emit $ Shift (length xs)
    emit Call
    modifyBaseOffset (subtract (length xs))
    emit $ Shove 1 1
compileExpr (Lambda xs e) = undefined
    
compileExpr (Let x e r) = do
    xOffset <- gets _baseOffset
    compileExpr e
    modify (localVarBaseIndices %~ insert x xOffset)
    compileExpr r
    emit $ Shove 1 1
    
compileExpr (Seq e1 e2) = do
            compileExpr e1
            emit $ Drop 1
            compileExpr e2
        
compileExpr (If c th el) = do
        labelElse <- newLabel
        labelEnd  <- newLabel

        compileExpr c
        prevOffset <- gets _baseOffset
        emit $ JumpLabelZ labelElse
        compileExpr th
        emit $ JumpLabel labelEnd

        modifyBaseOffset (const (prevOffset - 1))
        emit $ Label labelElse

        compileExpr el
        emit $ Label labelEnd
            
compileExpr (Source.Add e1 e2) = do
        compileExpr e1
        compileExpr e2
        emit Stack.Add

compileExpr (UnsafeSet x e) = do
    compileExpr e
    xPos <- getVarOffset x
    emit $ Set xPos

compileExpr (Source.LE x y) = do
        compileExpr x
        compileExpr y
        emit Stack.LE
    
compileExpr (Perform ix e) = do
    compileExpr e
    emit $ GetEffPrompt ix
    emit $ Yield

compileExpr (HandleEff ix e args res impl) = do
    yieldLabel  <- newLabel
    resultLabel <- newLabel

    emit $ Prompt yieldLabel resultLabel
    emit $ Dup
    emit $ SetEffPrompt ix
    
    -- Almost identical to `compileExpr (App e args)`, but
    -- manually pushes `resultLabel` as the return address and uses 'Jump' instead of 'Call'
    compileExpr e
    traverse compileExpr args
    emit $ PushLabel resultLabel
    emit $ Shift (length args)
    emit $ Jump

    emit $ Label yieldLabel

    -- If this is reached, the function has yielded
    xOffset <- gets _baseOffset                       -- Bind the argument variable (--TODO: This needs some manual offset manipulation)
    modify (localVarBaseIndices %~ insert res xOffset)--  ^
    modify (contBaseIndex .~ (xOffset + 1)) -- The continuation is passed *before* the argument, so the offset is 1 higher.
    compileExpr impl
    -- If control reaches this, `impl` has aborted. No idea what to do here yet...
    emit $ Panic "Aborting effects are NYI\n"

    -- if control reaches this, the function has finished without yielding
    emit $ Label resultLabel
    emit $ EndPrompt1
    emit $ RestoreEffPrompt ix

    modifyBaseOffset (subtract (length args))
    emit $ Shove 1 1

compileExpr (Continue e) = do
    emit . Shift =<< gets _contBaseIndex -- push the continuation
    compileExpr e -- evaluate the argument
    emit $ Resume

defineProc :: Members '[State CompState, Writer [Def]] r => Name -> Sem (State DeclState : Writer [Instr] : r) a -> Sem r a
defineProc name a = do
    (instrs, x) <- runWriter @[Instr] $ evalState initialDeclState a
    tell [DefProc name instrs]
    pure x

emit :: Members '[State DeclState, Writer [Instr], Writer [(Instr, Int)]] r => Instr -> Sem r ()
emit instr = do
    modifyBaseOffset (+ (instrOffset instr))
    offset <- gets _baseOffset
    tell [instr]
    tell [(instr, offset)]

instrOffset :: Instr -> Int
instrOffset = \case
    Push n          -> 1
    Drop n          -> -1
    Shift n         -> 1
    Swap            -> 0
    Dup             -> 1
    Set n           -> -1
    Shove n m       -> -m
    Jump            -> 0
    Call            -> 0 -- Call offset depends on called function (duh...)
    Label n         -> 0
    PushLabel _     -> 1
    JumpLabel n     -> 0
    JumpLabelZ n    -> -1
    Stack.Add       -> -1
    Stack.LE        -> -1
    (Prompt _ _)    -> 1  -- Switches stacks
    EndPrompt       -> -1
    EndPrompt1      -> -1
    Yield           -> -1 
    Resume          -> 0  -- Consumes a prompt, but also returns a result (?)
    GetEffPrompt _     -> 1
    SetEffPrompt _     -> -1
    RestoreEffPrompt _ -> 0 
    ProcAddress pr  -> 1
    Panic _         -> 0


emitAll :: Members '[State DeclState, Writer [Instr], Writer [(Instr, Int)]] r => [Instr] -> Sem r ()
emitAll = traverse_ emit

getVarOffset :: Members '[State DeclState] r => Name -> Sem r Int
getVarOffset x = do
    baseOffset <- gets _baseOffset
    mVar <- gets (lookup x . _localVarBaseIndices)
    case mVar of
        Nothing -> error $ "Non-existant variable '" <> x <> "'"
        Just vOffset -> pure (baseOffset - vOffset - 1)

newLabel :: Members '[State CompState] r => Sem r Int
newLabel = gets _nextLabel <* modify (nextLabel %~ (+ 1))


-}