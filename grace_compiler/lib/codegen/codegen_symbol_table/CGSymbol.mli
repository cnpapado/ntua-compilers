(* Symbol table *)
(* module Identifier = Symbol.Identifier *)

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
}

and variable_info = {                         (******* Μεταβλητή *******)
  llval   : Llvm.llvalue;                (**)
}

and function_info = {                         (******* Συνάρτηση  *******)
  llfun                      : Llvm.llvalue;
  mutable function_isForward : bool;          (* Forward Function       *)
  mutable function_paramlist : entry list;    (* List Parameters        *)
  mutable function_result    : Types.typ;     (* Function Result        *)
}

and parameter_info = {
  parameter_type           : Types.typ;
  parameter_mode           : Types.pass_mode;
  llp                      : Llvm.llvalue option;
}

and label_info = { 
  stepbb : Llvm.llbasicblock;
  afterbb : Llvm.llbasicblock;
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_label of label_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

val currentScope : scope ref              (* Τρέχουσα Εμβέλεια *)

val initSymbolTable  : int -> unit
val openScope        : unit -> unit
val closeScope       : unit -> unit
val newVariable      : Identifier.id -> Llvm.llvalue -> entry
val newFunction      : Identifier.id -> Llvm.llvalue -> entry * bool
val newParameter     : Identifier.id -> Types.typ -> Types.pass_mode -> Llvm.llvalue option -> entry -> entry
val newLabel         : Identifier.id -> Llvm.llbasicblock -> Llvm.llbasicblock -> entry

val forwardFunction      : entry -> unit
val endFunctionHeader    : entry -> Types.typ -> unit
val lookupEntry          : Identifier.id -> lookup_type -> bool -> entry