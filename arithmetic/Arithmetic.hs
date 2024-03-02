data Expr
  = Lit Int
  | BinOp Op Expr Expr
  | Let Id Type Expr Expr
  | Var Id

type Op = ()

type Type = ()

type Id = String
