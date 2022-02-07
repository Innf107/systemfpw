module Eff.Source where

import Eff.Prelude
import Data.Text qualified as T

-- An implementation of System F𝜖 extended with basic integer PrimOps

data Decl = Def Name Expr 
          | DefEff Name EffSig
          deriving (Show, Eq, Generic)

data Expr = EVal Value            -- v
          | App Expr Expr         -- e e
          | AppType Expr Type     -- e 𝜎

          | Let Name Type Expr Expr   -- let x : 𝜎 = e1 in e2 === (𝜆[𝜖] x : 𝜎. e2) e1
          -- Prim
          | Add Expr Expr         -- e + e
          | LE Expr Expr          -- e ≤ e
          | If Expr Expr Expr     -- if e then e else e
          deriving (Show, Eq, Generic)

          -- System F𝜖
data Value = Var Name                     -- x
          | Lambda EffRow Name Type Expr  -- 𝜆[𝜖] x : 𝜎. e
          | TyLambda Name Kind Value      -- Λ𝛼[𝜅] . v
          | Handler Handler               -- handler h
          | Perform Op EffRow [Type]      -- perform op 𝜖 𝜎*
          -- Prim
          | IntLit Int                   -- n
          deriving (Show, Eq, Generic)

newtype Handler = MkHandler [(Op, Expr)] -- { (opi ↦ fi)* }
                deriving (Show, Eq, Generic)

data Type = TyVar Name Kind             -- 𝛼[𝜅]
          | TyCon Name Kind [Type]      -- c[𝜅] 𝜎*
          | TFun Type EffRow Type       -- 𝜎 -{𝜖}> 𝜎
          | TForall Name Kind Type      -- ∀𝛼[𝜅] . 𝜎
          deriving (Show, Eq, Generic)

data EffRow = EffNil              -- ⟨⟩
            | EffCons Name EffRow -- ⟨l | 𝜖⟩
            deriving (Show, Eq, Generic)

data Kind = KType           -- ∗
          | KArr Kind Kind  -- 𝜅 → 𝜅
          | Lab             -- lab
          | Eff             -- eff
          deriving (Show, Eq, Generic)

newtype EffSig = MkEffSig [(Op, [(Name, Kind)], Type, Type)] -- { op_i : ∀𝛼_i*[𝜅_i]*. 𝜎_i → 𝜎′_i }
               deriving (Show, Eq, Generic)
type Op = Name -- ?

instance Pretty Decl where
  prettyIndent i (Def x e) = x <> " := " <> prettyIndent (i + 1) e
  prettyIndent i (DefEff ename esig) = "effect " <> ename <> " := " <> prettyIndent (i + 1) esig

instance Pretty Expr where
  prettyIndent i (EVal v) = prettyIndent i v
  prettyIndent i (App e1 e2) = "(" <> prettyIndent (i + 1) e1 <> " " <> prettyIndent (i + 1) e2 <> ")"
  prettyIndent i (AppType e ty) = "(" <> prettyIndent (i + 1) e <> " " <> prettyIndent (i + 1) ty <> ")"
  prettyIndent i (Let x ty e1 e2) = "(let " <> x <> " : " <> prettyIndent (i + 1) ty <> " =\n"
                                    <> indent (i + 1) <> prettyIndent (i + 2) e1
                                    <> indent i <> "in" <> prettyIndent i e2
  prettyIndent i (Add e1 e2) = "(" <> prettyIndent (i + 1) e1 <> " + " <> prettyIndent (i + 1) e2 <> ")"
  prettyIndent i (LE e1 e2) = "(" <> prettyIndent (i + 1) e1 <> " ≤ " <> prettyIndent (i + 1) e2 <> ")"
  prettyIndent i (If e1 e2 e3) = "(if " <> prettyIndent (i + 1) e1 
                              <> "\n" <> indent (i + 1) <> "then " <> prettyIndent (i + 2) e2
                              <> "\n" <> indent (i + 1) <> "else " <> prettyIndent (i + 2) e3 <> ")"

instance Pretty Value where
  prettyIndent i (Var x) = x
  prettyIndent i (Lambda eff x ty e) = "(𝜆[" <> prettyIndent (i + 1) eff <> "] "<> x <> " : " <> prettyIndent (i + 1) ty <> ". " <> prettyIndent (i + 1) e <> ")"
  prettyIndent i (TyLambda x k v) = "(Λ" <> x <> "[" <> prettyIndent (i + 1) k <> "]. " <> prettyIndent (i + 1) v
  prettyIndent i (Handler h) = "(handler " <> prettyIndent (i + 1) h <> ")"
  prettyIndent i (Perform op eff tys) = "(perform " <> op <> " " <> prettyIndent (i + 1) eff <> " " <> unwords (map (prettyIndent (i + 1)) tys) <> ")"
  prettyIndent i (IntLit n) = show n

instance Pretty Handler where
  prettyIndent i (MkHandler cases) = "{\n"
    <> unlines (map (\(o, f) -> indent (i + 1) <> o <> " |-> " <> prettyIndent (i + 1) f) cases)
    <> indent i <> "}"

instance Pretty Type where
  prettyIndent i (TyVar alpha k) = alpha <> "[" <> prettyIndent i k <> "]"
  prettyIndent i (TyCon c k args) = "(" <> c <> "[" <> prettyIndent i k <> "] " <> unwords (map (prettyIndent i) args) <> ")"
  prettyIndent i (TFun a EffNil b) = prettyIndent i a <> " -> " <> prettyIndent i b
  prettyIndent i (TFun a e b) = prettyIndent i a <> " -{" <> prettyIndent i e <> "}> " <> prettyIndent i b
  prettyIndent i (TForall a k t) = "(∀" <> a <> "[" <> prettyIndent i k <> "] " <> prettyIndent i t <> ")"

instance Pretty EffRow where 
  prettyIndent i EffNil = "⟨⟩"
  prettyIndent i (EffCons e es) = "⟨" <> e <> " | " <> prettyIndent i es <> "⟩"

instance Pretty Kind where
  prettyIndent i KType = "*"
  prettyIndent i (KArr a b) = "(" <> prettyIndent i a <> " -> " <> prettyIndent i b <> ")"
  prettyIndent i Lab = "lab"
  prettyIndent i Eff = "eff"


instance Pretty EffSig where

