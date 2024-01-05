exception SemError of string * Lexing.position
exception InternalSemError of string

val check_root : Ast.ParserAST.local_def -> Ast.SemAST.local_def 





