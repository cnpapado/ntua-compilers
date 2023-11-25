open Ast

exception SemError of string
exception InternalSemError of string

val check_root : ParserAST.ast_root -> SemAST.ast_root 

val check_header : ParserAST.header -> bool -> SemAST.header





