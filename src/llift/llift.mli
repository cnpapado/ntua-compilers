open Ast

val rename_ast : SemAST.local_def -> SemAST.local_def 
val replace_free : SemAST.local_def -> unit