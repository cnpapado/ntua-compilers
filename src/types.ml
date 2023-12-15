exception InternalTypeError of string

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
  
let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array {ttype = et1;size = 0}, TYPE_array {ttype = et2; size = _} (* alla stis typikes vs pragmatikes den to thelei *)
   | TYPE_array {ttype = et1;size = _}, TYPE_array {ttype = et2; size = 0} -> equalType et1 et2
   | TYPE_array {ttype = et1;size = sz1}, TYPE_array {ttype = et2; size = sz2} -> sz1 == sz2 && equalType et1 et2
   | TYPE_stringconst, TYPE_array {ttype = TYPE_char; size = _}            
   | TYPE_array {ttype = TYPE_char; size = _}, TYPE_stringconst            -> true 
   | _                                                                     -> t1 = t2

let equalType t1 t2 =
  match t1, t2 with
  | Some tt1, Some tt2 -> equalType tt1 tt2
  | _ -> raise (InternalTypeError "Expected type but got None")


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