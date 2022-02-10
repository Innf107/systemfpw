module Eff.Racket where

import Eff.Prelude

import Data.Text qualified as T

data RacketExpr =
    RDefine Name RacketExpr
  | RDefineF Name [Name] [RacketExpr]
  | RVar Name
  | RApp RacketExpr [RacketExpr]
  | RLambda [Name] [RacketExpr]
  | RLet [(Name, RacketExpr)] [RacketExpr]
  | RBegin [RacketExpr]

  | RSymbol Name
  | RIntLit Int
  | RHash [(RacketExpr, RacketExpr)]
  | RList [RacketExpr]

  | RAdd RacketExpr RacketExpr
  | RLE RacketExpr RacketExpr
  | RIf RacketExpr RacketExpr RacketExpr

  | RHashSet RacketExpr RacketExpr RacketExpr
  | RHashRef RacketExpr RacketExpr
  | RCadr Int RacketExpr -- RCadr n e ==> (ca[d * n]r e)  

  | RMakeContinuationPromptTag Name
  | RPromptAt RacketExpr RacketExpr
  | RControlAt RacketExpr Name RacketExpr
  deriving (Show, Eq)

instance Pretty RacketExpr where
  prettyIndent i (RDefine x e)     = "(define " <> x <> " " <> prettyIndent (i + 1) e <> ")"
  prettyIndent i (RDefineF x xs e) = "(define (" <> unwords (x:xs) <> ")\n" <> indent (i + 1) <> prettyIndent (i + 1) e <> ")"
  prettyIndent i (RVar x) = x
  prettyIndent i (RApp f xs) = "(" <> prettyIndent i f <> "\n" <> unlines (map (\e -> indent (i + 1) <> prettyIndent (i + 1) e) xs) <> indent i <> ")"
  prettyIndent i (RLambda xs es) = "(lambda " <> "(" <> unwords xs <> ") " <> prettyArgs i es <> ")"
  prettyIndent i (RLet binds es) = "(let* [\n" 
                                  <> unlines (map (\(x, e) -> indent (i + 1) <> "(" <> x <> " " <> prettyIndent (i + 1) e <> ")") binds)
                                  <> indent i <> "]\n"
                                  <> indent i <> prettyArgs i es <> ")"
  prettyIndent i (RBegin es) = "(begin\n" <> unlines (map (\e -> indent (i + 1) <> prettyIndent (i + 1) e) es) <> ")"

  prettyIndent i (RSymbol s) = "'" <> s
  prettyIndent i (RIntLit n) = show n
  prettyIndent i (RHash entries) = "(hash " <> prettyArgs i (concatMap (\(x, y) -> [x, y]) entries) <> ")"
  prettyIndent i (RList es) = "(list " <> prettyArgs i es <> ")"

  prettyIndent i (RAdd x y) = "(+ " <> prettyArgs i [x, y] <> ")"
  prettyIndent i (RLE x y) = "(<= " <> prettyArgs i [x, y] <> ")"
  prettyIndent i (RIf x y z) = "(if " <> prettyArgs i [x, y, z] <> ")"

  prettyIndent i (RHashSet h k v) = "(hash-set " <> prettyArgs i [h, k, v] <> ")"
  prettyIndent i (RHashRef h k)   = "(hash-ref " <> prettyArgs i [h, k] <> ")"
  prettyIndent i (RCadr j x)   = "(ca" <> T.replicate j "d" <> "r " <> prettyIndent (i + 1) x <> ")"

  prettyIndent i (RMakeContinuationPromptTag label) = "(make-continuation-prompt-tag '" <> label <> ")"
  prettyIndent i (RPromptAt p e) = "(reset0-at " <> prettyIndent i p <> prettyArgs i [e] <> ")"
  prettyIndent i (RControlAt p k e) = "(shift0-at " <> prettyIndent i p <> " " <> k <> prettyArgs i [e] <> ")"

prettyArgs :: Int -> [RacketExpr] -> Text
prettyArgs i es = "\n" <> unlines (map (\e -> indent (i + 1) <> prettyIndent (i + 1) e) es) <> indent i


prettyRacketProgram :: [RacketExpr] -> Text
prettyRacketProgram es = unlines [
                          "#lang racket\n" 
                        , "(require racket/control)"
                        , unlines (map pretty es) 
                        , "(println (main (hash) 0))"
                        ]
