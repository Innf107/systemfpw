module Eff.Source where

import Eff.Prelude
import Data.Text qualified as T

{-
Untyped
There is *no* name resolution, so all names are effectively global
Effects are represented by an action index

handleExpr (perform 0 42) {
    0 x -> x + 1;
    1 x -> -1;
}
= 43

-}

data Decl = DefFun Name [Name] Expr deriving (Show, Eq, Generic)

        -- LC
data Expr = IntLit Int
          | Var Name
          | App Expr [Expr]
          | Lambda [Name] Expr -- Non-capturing lambda
          | Let Name Expr Expr
          | Seq Expr Expr
          | If Expr Expr Expr -- if 0 _ z = z; if _ y _ = y
        -- PrimOps
          | Add Expr Expr
          | UnsafeSet Name Expr
          | LE Expr Expr
        -- Effects
          | Perform Int Expr
          | HandleEff Int Expr [Expr] Name Expr
          | Continue Expr -- Should only be called in a HandleEff clause. Abort is simply handled by *not* calling Continue.
          --  | Reify Name -- Captures the continuation (by copying the call stack)
          deriving (Show, Eq, Generic)

instance Pretty Decl where
    prettyIndent i (DefFun f xs e) = f <> "(" <> T.intercalate ", " xs <> ") = \n" <> indent (i + 1) <> prettyIndent (i + 1) e

instance Pretty Expr where
    prettyIndent i (IntLit n) = show n
    prettyIndent i (Var x) = x
    prettyIndent i (App f xs) = prettyIndent i f <> "(" <> T.intercalate ", " (map (prettyIndent (i + 1)) xs) <> ")"
    prettyIndent i (Lambda xs e) = "(\\(" <> T.intercalate ", " xs <> ") -> " <> prettyIndent (i + 1) e <> ")"
    prettyIndent i (Let x e1 e2) = "let " <> x <> " = " <> prettyIndent (i + 1) e1 <> "\n" <> indent i <> "in " <> prettyIndent i e2
    prettyIndent i (Seq e1 e2) = "seq " <> prettyIndent (i + 1) e1 <> "\n" <> indent i <> "in " <> prettyIndent (i + 1) e2
    prettyIndent i (If c e1 e2) = "if " <> prettyIndent (i + 1) c <> "\n" <> indent i <> "then \n" <> indent (i + 1) <> prettyIndent (i + 1) e1 <> "\n" <> indent i <> "else \n" <> indent (i + 1) <> prettyIndent (i + 1) e2 
    prettyIndent i (Add x y) = "(" <> prettyIndent i x <> " + " <> prettyIndent i y <> ")"
    prettyIndent i (UnsafeSet x e) = "unsafeSet x = " <> prettyIndent (i + 1) e
    prettyIndent i (LE x y) = "(" <> prettyIndent i x <> " <= " <> prettyIndent i y <> ")"
    prettyIndent i (Perform action e) = "perform " <> show action <> " with " <> prettyIndent (i + 1) e
    prettyIndent i (HandleEff eff e args res action) = "handle[" <> show eff <> "] " <> prettyIndent (i + 1) e <> "(" <> T.intercalate ", " (map (prettyIndent (i + 1)) args) <> ") with " <>
      res <> "-> \n" <> indent (i + 1) <> prettyIndent (i + 1) action
    prettyIndent i (Continue e) = "continue " <> prettyIndent (i + 1) e
