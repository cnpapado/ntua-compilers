open Ast

(* module SS = Set.Make(String) *)

val rename_ast : SemAST.local_def -> SemAST.local_def 
val replace_free : SemAST.local_def -> SemAST.local_def