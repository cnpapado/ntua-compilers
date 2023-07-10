type typ = | TYPE_int        
           | TYPE_char        
           | TYPE_array of {ttype: typ; size: int}

let rec sizeOfType t =  
  match t with
  | TYPE_int                           -> 2
  | TYPE_char                          -> 1
  | TYPE_array {ttype = et; size = sz} -> sz * sizeOfType et
  
let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array {ttype = et1;size = _}, TYPE_array {ttype = et2; size = _} -> equalType et1 et2
   | _                                                                     -> t1 = t2