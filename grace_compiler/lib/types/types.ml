exception InternalTypeError of string

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type typ = | TYPE_int        
           | TYPE_char        
           | TYPE_array of {ttype: typ; size: int}
           | TYPE_uninitialized (* function return before properly initialized *)
           | TYPE_nothing       (* artificial function return type for procedures *)
           | TYPE_stringconst   (* constant string appearing only in lvals *)
           | TYPE_bool          (* artificial type for conditions *)

let rec sizeOfType t =  
  match t with
  | TYPE_int                           -> 2
  | TYPE_char                          -> 1
  | TYPE_array {ttype = et; size = sz} -> sz * sizeOfType et (* except if sz=0 for a[] *)
  
let rec equalType ?(flexible_on_autocomplete=false) t1 t2 =
  match t1, t2 with
   | TYPE_array {ttype = et1; size = 0}, TYPE_array {ttype = et2; size = _} when flexible_on_autocomplete=true
                                                                                -> equalType et1 et2
   | TYPE_array {ttype = et1; size = _}, TYPE_array {ttype = et2; size = 0} when flexible_on_autocomplete=true 
                                                                                -> equalType et1 et2
   | TYPE_array {ttype = et1; size = sz1}, TYPE_array {ttype = et2; size = sz2} -> sz1 == sz2 && equalType et1 et2
   | TYPE_stringconst, TYPE_array {ttype = TYPE_char; size = _}                       
   | TYPE_array {ttype = TYPE_char; size = _}, TYPE_stringconst                 -> true 
   | _                                                                          -> t1 = t2

let equalType ?(flexible_on_autocomplete=false) t1 t2 =
  match t1, t2 with
  | Some tt1, Some tt2 -> equalType ~flexible_on_autocomplete:flexible_on_autocomplete tt1 tt2
  | _ -> raise (InternalTypeError "Expected some type but got None")


let rec pp_typ t = 
  match t with 
  | Some tt -> (
    match tt with
    | TYPE_int -> "int"        
    | TYPE_char -> "char"        
    | TYPE_array {ttype;size=_} -> "array" ^ "[" ^ pp_typ (Some ttype) ^ "]"
    | TYPE_uninitialized -> "uninit" 
    | TYPE_nothing -> "nothing" 
    | TYPE_stringconst -> "strconst"
    | TYPE_bool -> "bool" )
  | None -> "none"


let rec get_base_arr_typ t = 
  match t with 
  | TYPE_array {ttype;size=_} -> get_base_arr_typ ttype
  | _ -> t

let arr_dim t = 
  let rec arr_dim t n = 
    match t with 
    | TYPE_array {ttype;size=_} -> arr_dim ttype n+1 
    | _ -> n
  in arr_dim t 0