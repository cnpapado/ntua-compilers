type expr = Int of int
            | BinExpr of binop*expr*expr 
            | BinAssign of assignop*expr*expr
            | UnaryExpr of unaryop*expr
            | UnaryAssign of unaryassignop*expr

and binop =    Times | Div | Mod
               | Plus | Minus | Lt | Gt | Le | Ge
               | Eq | Neq | And | Or | Comma

and assignop = Assign | TimesEq | DivEq
                | ModEq | PlusEq | MinusEq

and unaryop = BitAnd | UTimes | UPlus | UMinus | BitNot

and unaryassignop = PrePlusPlus | PostPlusPlus
                    | PreMinusMinus | PostMinusMinus