open Ast

exception SemError of string * Lexing.position
exception InternalSemError of string

val check_root : ParserAST.local_def -> SemAST.local_def 

val check_header : ParserAST.header -> bool -> SemAST.header





