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
  | TYPE_array {ttype = et; size = sz} -> sz * sizeOfType et
  
let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array {ttype = et1;size = _}, TYPE_array {ttype = et2; size = _} -> equalType et1 et2
   | _                                                                     -> t1 = t2