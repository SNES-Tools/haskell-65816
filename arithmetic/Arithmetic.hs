-- Syntax (cf. Section 4.1)

data Expr
  = Lit Int
  | BinOp Op Expr Expr
  | Let Id Type Expr Expr
  | Var Id

data Op = Plus | Minus

type Type = ()

type Id = String
