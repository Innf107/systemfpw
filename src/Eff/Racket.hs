module Eff.Racket where

import Eff.Prelude

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
  | RHash

  | RAdd RacketExpr RacketExpr
  | RLE RacketExpr RacketExpr
  | RIf RacketExpr RacketExpr RacketExpr

  | RHashSet RacketExpr RacketExpr RacketExpr

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
  prettyIndent i (RLet binds es) = "(let [\n" 
                                  <> unlines (map (\(x, e) -> indent (i + 1) <> "(" <> x <> " " <> prettyIndent (i + 1) e <> ")") binds)
                                  <> indent i <> "]\n"
                                  <> indent i <> prettyArgs i es <> ")"

  prettyIndent i (RSymbol s) = "'" <> s
  prettyIndent i (RIntLit n) = show n
  prettyIndent i (RHash) = "(hash)"

  prettyIndent i (RAdd x y) = "(+ " <> prettyArgs i [x, y] <> ")"
  prettyIndent i (RLE x y) = "(<= " <> prettyArgs i [x, y] <> ")"
  prettyIndent i (RIf x y z) = "(if " <> prettyArgs i [x, y, z] <> ")"

  prettyIndent i (RMakeContinuationPromptTag) = "(make-continuation-prompt-tag)"
  prettyIndent i (RPromptAt p e) = "(prompt-at " <> prettyIndent i p <> prettyArgs i [e] <> ")"
  prettyIndent i (RControlAt p k e) = "(control-at " <> prettyIndent i p <> " " <> k <> prettyArgs i [e] <> ")"

prettyArgs :: Int -> [RacketExpr] -> Text
prettyArgs i es = "\n" <> unlines (map (\e -> indent (i + 1) <> prettyIndent (i + 1) e) es) <> indent i


prettyRacketProgram :: [RacketExpr] -> Text
prettyRacketProgram es = "#lang racket\n" 
                      <> unlines (map pretty es) 
                      <> "\n(println (main 0))"
