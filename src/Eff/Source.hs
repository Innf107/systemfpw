module Eff.Source where

import Eff.Prelude
import Data.Text qualified as T

-- An implementation of System Fđ extended with basic integer PrimOps

data Decl = Def Name Expr 
          | DefEff Name EffSig
          deriving (Show, Eq, Generic)

data Expr = EVal Value            -- v
          | App Expr Expr         -- e e
          | AppType Expr Type     -- e đ

          | Let Name Type Expr Expr   -- let x : đ = e1 in e2 === (đ[đ] x : đ. e2) e1
          -- Prim
          | Add Expr Expr         -- e + e
          | LE Expr Expr          -- e â€ e
          | If Expr Expr Expr     -- if e then e else e
          deriving (Show, Eq, Generic)

          -- System Fđ
data Value = Var Name                     -- x
          | Lambda EffRow Name Type Expr  -- đ[đ] x : đ. e
          | TyLambda Name Kind Value      -- ÎđŒ[đ] . v
          | Handler Handler               -- handler h
          | Perform Name Op EffRow [Type]      -- perform[l] op đ đ*
          -- Prim
          | IntLit Int                   -- n
          deriving (Show, Eq, Generic)

data Handler = MkHandler Name [(Op, (Name, Name, Expr))] -- [l]{ (opi âŠ fi)* }
                deriving (Show, Eq, Generic)

data Type = TyVar Name Kind             -- đŒ[đ]
          | TyCon Name Kind [Type]      -- c[đ] đ*
          | TFun Type EffRow Type       -- đ -{đ}> đ
          | TForall Name Kind Type      -- âđŒ[đ] . đ
          deriving (Show, Eq, Generic)

data EffRow = EffNil              -- âšâ©
            | EffCons Name EffRow -- âšl | đâ©
            deriving (Show, Eq, Generic)

data Kind = KType           -- â
          | KArr Kind Kind  -- đ â đ
          | Lab             -- lab
          | Eff             -- eff
          deriving (Show, Eq, Generic)

newtype EffSig = MkEffSig [(Op, [(Name, Kind)], Type, Type)] -- { op_i : âđŒ_i*[đ_i]*. đ_i â đâČ_i }
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
  prettyIndent i (LE e1 e2) = "(" <> prettyIndent (i + 1) e1 <> " â€ " <> prettyIndent (i + 1) e2 <> ")"
  prettyIndent i (If e1 e2 e3) = "(if " <> prettyIndent (i + 1) e1 
                              <> "\n" <> indent (i + 1) <> "then " <> prettyIndent (i + 2) e2
                              <> "\n" <> indent (i + 1) <> "else " <> prettyIndent (i + 2) e3 <> ")"

instance Pretty Value where
  prettyIndent i (Var x) = x
  prettyIndent i (Lambda eff x ty e) = "(đ[" <> prettyIndent (i + 1) eff <> "] "<> x <> " : " <> prettyIndent (i + 1) ty <> ". " <> prettyIndent (i + 1) e <> ")"
  prettyIndent i (TyLambda x k v) = "(Î" <> x <> "[" <> prettyIndent (i + 1) k <> "]. " <> prettyIndent (i + 1) v
  prettyIndent i (Handler h) = "(handler " <> prettyIndent (i + 1) h <> ")"
  prettyIndent i (Perform l op eff tys) = "(perform[" <> l <> "] " <> op <> " " <> prettyIndent (i + 1) eff <> " " <> unwords (map (prettyIndent (i + 1)) tys) <> ")"
  prettyIndent i (IntLit n) = show n

instance Pretty Handler where
  prettyIndent i (MkHandler l cases) = "[" <> l <> "]{\n"
    <> unlines (map (\(o, (k, v, e)) -> indent (i + 1) <> o <> " |-> đ" <> k <> " " <> v <> ". " <> prettyIndent (i + 1) e) cases)
    <> indent i <> "}"

instance Pretty Type where
  prettyIndent i (TyVar alpha k) = alpha <> "[" <> prettyIndent i k <> "]"
  prettyIndent i (TyCon c k args) = "(" <> c <> "[" <> prettyIndent i k <> "] " <> unwords (map (prettyIndent i) args) <> ")"
  prettyIndent i (TFun a EffNil b) = prettyIndent i a <> " -> " <> prettyIndent i b
  prettyIndent i (TFun a e b) = prettyIndent i a <> " -{" <> prettyIndent i e <> "}> " <> prettyIndent i b
  prettyIndent i (TForall a k t) = "(â" <> a <> "[" <> prettyIndent i k <> "] " <> prettyIndent i t <> ")"

instance Pretty EffRow where 
  prettyIndent i EffNil = "âšâ©"
  prettyIndent i (EffCons e es) = "âš" <> e <> " | " <> prettyIndent i es <> "â©"

instance Pretty Kind where
  prettyIndent i KType = "*"
  prettyIndent i (KArr a b) = "(" <> prettyIndent i a <> " -> " <> prettyIndent i b <> ")"
  prettyIndent i Lab = "lab"
  prettyIndent i Eff = "eff"


instance Pretty EffSig where
  prettyIndent i (MkEffSig ops) = "{\n" <> unlines (map prettyOpSig ops) <> "}"
    where
      prettyOpSig (op, [], inTy, outTy)  = indent (i + 1) <> op <> " : " <> prettyIndent (i + 1) inTy <> " -> " <> prettyIndent (i + 1) outTy
      prettyOpSig (op, tvs, inTy, outTy) = indent (i + 1) <> op <> " : â" <> unwords (map (\(alpha, k) -> alpha <> "[" <> prettyIndent (i + 1) k <> "]") tvs) <> ". "  <> prettyIndent (i + 1) inTy <> " -> " <> prettyIndent (i + 1) outTy
