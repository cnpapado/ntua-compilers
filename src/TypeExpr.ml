open Core
open Error
open Identifier
open Types
open Symbol
open Symbtest
open PrettyPrint
open Ast

let rec type_expr id expected_entry_typ how = 
  let e = Symbol.lookupEntry (id_make id) expected_entry_typ how true in
  match e.entry_info with 
        | ENTRY_variable { variable_type = t;
                          variable_offset = _}  -> t
        | ENTRY_function { function_isForward = _; function_paramlist = _ ;
                           function_redeflist = _ ;
                           function_result    = t ; 
                           function_pstatus   = _ ; 
                           function_initquad  = _ } -> t
        | _ -> ignore(error "this should never be printed\n"); TYPE_none

and check_bin_operator op t1 t2 = 
 match op with
   | Times
   | Div
   | Mod
   | Plus
   | Minus -> arithmetic_type t1 t2
   | Lt
   | Gt
   | Le
   | Ge    -> comparison_type t1 t2
   | Eq    -> if(equalType t1 t2) then t1 else (ignore(error "Error eq operator\n");TYPE_none)
   | Comma -> t2
   | Neq
   | And
   | Or    -> bool_type t1 t2

and arithmetic_type t1 t2 = 
    match t1, t2 with
    | (TYPE_int, TYPE_int) -> TYPE_int      
    | (TYPE_double, TYPE_double) -> TYPE_double
    | (TYPE_int, TYPE_double) -> TYPE_double  
    | (TYPE_double, TYPE_int) -> TYPE_double
    | (TYPE_ptr {ttype= t ; level = l}, TYPE_int) -> t
    | (_,_) -> ignore(error "Expected different types arithmetic op\n"); TYPE_none
    
and comparison_type t1 t2 = 
    (* 
       t* remains to be resolved 
    *)
    match t1, t2 with
    | (TYPE_int, TYPE_int)      
    | (TYPE_double, TYPE_double)
    | (TYPE_bool, TYPE_bool)    
    | (TYPE_int, TYPE_double)   
    | (TYPE_double, TYPE_int) -> TYPE_bool 
    | (_,_) -> ignore(error "Expected different types comparison op"); TYPE_none

and bool_type t1 t2 = 
    match (t1, t2) with    
    | (TYPE_bool, TYPE_bool) -> TYPE_bool 
    | (_,_) -> ignore(error "Expected different types bool op"); TYPE_none

and check_unary_operator op t = 
    match op with
     | BitAnd -> 
           let f t =
             match t with
             | TYPE_int -> 0
             | TYPE_ptr {ttype=_; level=l} -> l+1
           in
             TYPE_ptr {ttype=t; level=(f t)}
     | UTimes -> TYPE_none
     | UPlus  
     | UMinus -> sign_type t
     | BitNot -> 
          let f t = 
            match t with 
            |TYPE_bool -> TYPE_bool
            | _        -> (ignore(error "Operator ! being used on wrong type!\n"); TYPE_none)
          in
            f t

and sign_type t = 
    match t with
     | TYPE_int    -> TYPE_int
     | TYPE_double -> TYPE_double
